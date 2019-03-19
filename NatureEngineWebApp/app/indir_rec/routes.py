"""routes.py: A group of handlers for routes relating to indirect reciprocity"""

from flask import render_template, url_for, request, jsonify, current_app
from app.indir_rec import bp
import requests
from ..models import ReputationCommunity, ReputationGeneration, ReputationPlayer, ReputationStrategy,\
    ReputationActionOnlookers
from app import db
import random
from sqlalchemy.sql import func
from flask_login import current_user
from rq.job import Job
from typing import Dict, List, Any, Tuple
from .action_logic import ActionType


@bp.route('/reputation', methods=['GET', 'POST'])
def reputation():
    """The handler for the route to set up of reputation games"""
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    if request.method == 'GET':
        # Handle sending the web page with the form for setting up a reputation game
        return render_template('reputation.html', title='Reputation', strategies=strategies)
    if request.method == 'POST':
        # Validate form data
        form_data = request.get_json()
        strategy_counts = form_data['strategy_counts']
        player_count = 0
        for strategy in strategy_counts:
            player_count += strategy['count']
        if 4 < player_count < 200 and 0 <= int(form_data['num_of_onlookers']) \
                and 2 <= int(form_data['num_of_generations']) <= 20 \
                and 5 <= int(form_data['length_of_generations']) <= 200 \
                and 0 <= float(form_data['mutation_chance']) <= 1:
            # Set up community in database
            community = ReputationCommunity(simulated=False, timed_out=False)
            db.session.add(community)
            db.session.commit()
            # Put the game into the task queue
            if current_user.is_authenticated:
                job = current_app.task_queue.enqueue('app.indir_rec.run_game.reputation_run', strategy_counts,
                                               int(form_data['num_of_onlookers']), int(form_data['num_of_generations']),
                                               int(form_data['length_of_generations']),
                                               float(form_data['mutation_chance']),
                                               community.id, user_id=current_user.id, label=form_data['label'])
            else:
                job = current_app.task_queue.enqueue('app.indir_rec.run_game.reputation_run', strategy_counts,
                                               int(form_data['num_of_onlookers']), int(form_data['num_of_generations']),
                                               int(form_data['length_of_generations']),
                                               float(form_data['mutation_chance']), community.id)
            # Send details to redirect if appropriate
            return jsonify({'url': url_for('indir_rec.reputation_finished', reputation_id=community.id,
                                           job_id=job.get_id())})
        # If form data doesn't validate return to form page
        return render_template('reputation.html', title='Reputation', strategies=strategies)


@bp.route('/is_reputation_finished/<reputation_id>/<job_id>')
def is_reputation_finished(reputation_id, job_id):
    """
    The route to ping to check whether a reputation game has been finished or not
    :param reputation_id: The id of the reputation game from the database
    :param job_id: The id of the job in the queue running this game
    :return: A json stating whether to redirect or not
    """
    community = ReputationCommunity.query.filter_by(id=reputation_id).first_or_404()
    # Detect if finished or failed
    if community.is_finished():
        finished = True
    elif Job(job_id, current_app.redis).is_failed:
        community.timed_out = True
        db.session.commit()
        finished = True
    else:
        finished = False
    # return url to redirect to, when finished
    return jsonify({'finished': finished,
                    'url': url_for('indir_rec.reputation_finished', reputation_id=reputation_id)})


@bp.route('/reputation_finished/<reputation_id>/', defaults={'job_id': None})
@bp.route('/reputation_finished/<reputation_id>/<job_id>')
def reputation_finished(reputation_id, job_id):
    """
    The handler for the route when a game is running or has finished
    :param reputation_id: The database id of the reputation
    :param job_id: The id of the job running the game (defaults to None)
    :return: The rendered template to serve to the client
    """
    # Get the strategies in the agents service and the community from the database
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    community: ReputationCommunity = ReputationCommunity.query.filter_by(id=reputation_id).first_or_404()
    if community.timed_out:
        # Notify user of timeout
        return render_template('reputation_timed_out.html', title='Reputation Timed Out', reputation_id=reputation_id)
    if community.is_finished():
        #
        population_chart_data, strategy_colours = get_population_chart_data_and_strategy_colours(community)
        measurement_chart_data = get_measurements_chart_data(community)
        community_fitness_stats = db.session.query(func.max(ReputationCommunity.fitness).label("max_fit"),
                                                   func.min(ReputationCommunity.fitness).label("min_fit"),
                                                   func.avg(ReputationCommunity.fitness).label("avg_fit")).one()
        timepoints = [timepoint for timepoint in range(community.length_of_generations)]
        players, actions = get_players_and_actions(community)
        num_of_players_per_gen = len(players[0])
        return render_template('reputation_finished.html', title='Reputation Finished', strategies=strategies,
                               community=community, action_type=ActionType, population_chart_data=population_chart_data,
                               measurement_chart_data=measurement_chart_data,
                               fitness_chart_data=get_fitness_chart_data(community),
                               lowest_fitness=community_fitness_stats.min_fit,
                               highest_fitness=community_fitness_stats.max_fit,
                               average_fitness=round(community_fitness_stats.avg_fit), timepoints=timepoints,
                               strategy_colours=strategy_colours, num_of_players_per_gen=num_of_players_per_gen,
                               players=players, actions=actions)
    elif job_id is not None:
        # Detect if timed out and set that into the database, or show that game is still running to the user
        if Job(job_id, current_app.redis).is_failed:
            community.timed_out = True
            db.session.commit()
            return render_template('reputation_timed_out.html', title='Reputation Timed Out',
                                   reputation_id=reputation_id)
        else:
            return render_template('reputation_running.html', title='Reputation Running', reputation_id=reputation_id,
                                   job_id=job_id)
    else:
        # Show the user that the game is still running
        return render_template('reputation_running.html', title='Reputation Running', reputation_id=reputation_id,
                               job_id=job_id)


def get_players_and_actions(community: ReputationCommunity) \
        -> Tuple[Dict[int, Dict[int, Dict[str, int]]], Dict[int, Dict[int, List[Dict]]]]:
    """
    Get the players and their stats (indexed by generation) and
    actions of each player (indexed by generation) in format that is javascript readable (json-convertible).
    First layer of dictionary keys is the generation ids, second is the player ids.
    :param community: the community to get the players and actions from
    :type community: ReputationCommunity
    :return: the players and their stats, and the players actions
    :rtype: Tuple[Dict[int, Dict[int, Dict[str, int]]], Dict[int, Dict[int, List[Dict]]]]
    """
    players: Dict[int, Dict[int, Dict[str, int]]] = {}
    actions: Dict[int, Dict[int, List[Dict]]] = {}
    for generation in community.generations:
        gen_players: Dict[int, Dict[str, int]] = {}
        gen_actions: Dict[int, List[Dict]] = {}
        for player in generation.players:
            # add the players stats in the players dictionary
            gen_players[player.player_id] = {'cooperation_rate': player.cooperation_rate,
                                             'social_activeness': player.social_activeness,
                                             'positivity_of_gossip': player.positivity_of_gossip,
                                             'fitness': player.fitness, 'strategy': player.strategy}
            # make action data json-convertible and add it to the actions dictionary
            for action in player.actions:
                if action.type is ActionType.INTERACTION:
                    onlookers = ReputationActionOnlookers.query.filter_by(action_id=action.id).all()
                    onlooker_ids = []
                    for onlooker in onlookers:
                        onlooker_ids.append(ReputationPlayer.query.filter_by(id=onlooker.onlooker_id).first().player_id)
                    if action.timepoint % community.length_of_generations in gen_actions:
                        gen_actions[action.timepoint % community.length_of_generations].append({'type': 'interaction',
                                                              'donor': action.get_donor_player_id(),
                                                              'recipient': action.get_recipient_player_id(),
                                                              'action': str(action.action), 'reason': action.reason,
                                                              'onlookers': onlooker_ids})
                    else:
                        gen_actions[action.timepoint % community.length_of_generations] = [{'type': 'interaction',
                                                         'donor': action.get_donor_player_id(),
                                                         'recipient': action.get_recipient_player_id(),
                                                         'action': str(action.action), 'reason': action.reason,
                                                         'onlookers': onlooker_ids}]
                elif action.type is ActionType.GOSSIP:
                    if action.timepoint % community.length_of_generations in gen_actions:
                        gen_actions[action.timepoint % community.length_of_generations].append({'type': 'gossip',
                                                              'gossiper': action.get_gossiper_player_id(),
                                                              'about': action.get_about_player_id(),
                                                              'recipient': action.get_recipient_player_id(),
                                                              'gossip': str(action.gossip), 'reason': action.reason})
                    else:
                        gen_actions[action.timepoint % community.length_of_generations] = [{'type': 'gossip', 'gossiper': action.get_gossiper_player_id(),
                                                          'about': action.get_about_player_id(),
                                                          'recipient': action.get_recipient_player_id(),
                                                          'gossip': str(action.gossip), 'reason': action.reason}]
        players[generation.generation_id] = gen_players
        actions[generation.generation_id] = gen_actions
    return players, actions


def get_population_chart_data_and_strategy_colours(community: ReputationCommunity):
    """
    Get the data in the correct format for graph.js to create the population line chart on the reputation finished
    page and also the colours for each strategy
    :param community: The community to build the graph data for and get the strategy colours for
    :type community: ReputationCommunity
    :return: The chart data for the population chart and the strategy colours
    """
    # Create the dictionary for the chart data to populate
    chart_data = {'type': 'line', 'data': {'datasets': [], 'labels': []},
                  'options': {'title': {'display': True, 'text': "Population fluctuation across the generations"},
                              'scales': {'yAxes': [{'scaleLabel': {'display': True, 'labelString': "Strategy Count"}}],
                                         'xAxes': [{'scaleLabel': {'display': True, 'labelString': "Generation"}}]}}}
    strategy_colours: Dict[int, Dict[str, Any]] = {}
    created_datasets = []
    hex_digits = list("0123456789ABCDEF")
    for generation in community.generations:
        gen: ReputationGeneration = generation
        chart_data['data']['labels'].append(gen.generation_id)
        for player in generation.players:
            strategy = ReputationStrategy.query.filter_by(id=player.strategy).first()
            if strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +\
                    strategy.trust_model + " " + strategy.options not in created_datasets:
                created_datasets.append(strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                    strategy.trust_model + " " + strategy.options)
                # Add to strategy colours
                hex_colour = "#" + ''.join([hex_digits[random.randint(0, len(hex_digits) - 1)] for _ in range(6)])
                # Add to chart data
                chart_data['data']['datasets']. \
                    append({'label': strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                     strategy.trust_model + " " + strategy.options,
                            'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
                            'borderColor': hex_colour, 'backgroundColor': hex_colour})
                chart_data['data']['datasets'][-1]['data'][gen.generation_id] += 1
                strategy_colours[player.strategy] = {'colour': hex_colour, 'strategy': strategy.donor_strategy,
                                                     'non_donor_strategy': strategy.non_donor_strategy,
                                                     'trust_model': strategy.trust_model,
                                                     'options': strategy.options}
            else:
                for dataset in chart_data['data']['datasets']:
                    if dataset['label'] == strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +\
                            strategy.trust_model + " " + strategy.options:
                        dataset['data'][gen.generation_id] += 1
                        break
    return chart_data, strategy_colours


def get_measurements_chart_data(community: ReputationCommunity):
    """
    Get the chart data for the cooperation rate, social activeness and positivity of gossip chart
    :param community: The community to build the chart data for
    :type community: ReputationCommunity
    :return: The chart data for the cooperation rate, social activeness and positivity of gossip chart
    """
    # Set up chart data outline to add to
    chart_data = {'type': 'bar', 'data': {'datasets': [
        {'label': "Cooperation rate", 'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
         'backgroundColor': "#0074D9",
         'id': "cooperation_rate"},
        {'label': "Social activeness", 'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
         'backgroundColor': "#B10DC9",
         'id': "social_activeness"},
        {'label': "Positivity of gossip", 'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
         'backgroundColor': "#39CCCC",
         'id': "positivity_of_gossip"}
    ], 'labels': []},
                  'options': {'title': {'display': True,
                                        'text': "Measurements over the generations"},
                              'scales': {'yAxes': [{'ticks': {'max': 100, 'min': 0},
                                                    'scaleLabel': {'display': True,
                                                                   'labelString': "Measurement value"}}],
                                         'xAxes': [{'scaleLabel': {'display': True,
                                                                   'labelString': "Generation"}}]}}}
    # Add data to chart data for measurements
    for generation in community.generations:
        gen: ReputationGeneration = generation
        chart_data['data']['labels'].append(gen.generation_id)
        for dataset in chart_data['data']['datasets']:
            if dataset['label'] == "Cooperation rate":
                dataset['data'][gen.generation_id] = gen.cooperation_rate
            elif dataset['label'] == "Social activeness":
                dataset['data'][gen.generation_id] = gen.social_activeness
            elif dataset['label'] == "Positivity of gossip":
                dataset['data'][gen.generation_id] = gen.positivity_of_gossip
    return chart_data


def get_fitness_chart_data(community: ReputationCommunity):
    """
    Get the data for graph.js for the fitness chart of this community and how it fluctuates over the generations
    :param community: The community to get the chart data for
    :return: The chart data for the fitness of each generation
    """
    return {'type': 'bar', 'data': {'labels': [gen.generation_id for gen in community.generations],
                                    'datasets': [{'label': "Fitness",
                                                  'data': [gen.fitness for gen in community.generations],
                                                  'backgroundColor': "#85144b"}],
                                    'options': {'title': {'display': True,
                                                          'text': "Fitness over the generations"},
                                                'scales': {'yAxes': [{'scaleLabel': {'display': True,
                                                                                     'labelString': "Fitness"}}],
                                                           'xAxes': [{'scaleLabel': {'display': True,
                                                                                     'labelString': "Generation"}}]}}}}


@bp.route('/reputation_historical')
def reputation_historical():
    """
    The handler for the route that deals with displaying data on historical reputation games in the system
    :return: The rendered template to send to the client
    """
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    social_vs_cooperation_rate_chart_data = get_social_vs_cooperation_rate_chart_data()
    gen_length_vs_cooperation_rate_chart_data = get_gen_length_vs_cooperation_rate_chart_data()
    cooperation_rate_vs_social_welfare_chart_data = get_cooperation_rate_vs_social_welfare_chart_data()
    strategies_vs_cooperation_rate_chart_data = get_strategies_vs_cooperation_rate_chart_data()
    return render_template('reputation_historical.html', title='Reputation Games Historical Data', strategies=strategies,
                           social_vs_cooperation_rate_chart_data=social_vs_cooperation_rate_chart_data,
                           gen_length_vs_cooperation_rate_chart_data=gen_length_vs_cooperation_rate_chart_data,
                           cooperation_rate_vs_social_welfare_chart_data=cooperation_rate_vs_social_welfare_chart_data,
                           strategies_vs_cooperation_rate_chart_data=strategies_vs_cooperation_rate_chart_data)


def get_social_vs_cooperation_rate_chart_data():
    """
    Get the data for the chart on the historical reputation game data page, that compares the social activeness and
    positivity of gossip of a community to the cooperation rate of that community
    :return: The chart data
    """
    communities = ReputationCommunity.query.filter_by(corrupted_observations=False, simulated=True).all()
    chart_data = {'type': 'scatter',
                  'data': {'datasets': [
                      {
                          'label': "Social Activeness vs. Cooperation Rate",
                          'data': [],
                          'backgroundColor': "#B10DC9", 'borderColor': "#0074D9"
                      },
                      {
                          'label': "Positivity Of Gossip vs. Cooperation Rate",
                          'data': [],
                          'backgroundColor': "#39CCCC", 'borderColor': "#0074D9"
                      }
                  ]
                           },
                  'options': {
                      'title': {'display': True,
                                'text': "A scatter of the social measurements vs the cooperation rate of each community"},
                      'scales': {'yAxes': [{'ticks': {'max': 100, 'min': 0},
                                            'scaleLabel': {'display': True, 'labelString': "Cooperation Rate"}}],
                                 'xAxes': [{'ticks': {'max': 100, 'min': 0},
                                            'scaleLabel': {'display': True, 'labelString': "Social Measurement"}}]}
                  }
                  }
    for community in communities:
        chart_data['data']['datasets'][0]['data'].append({'x': community.social_activeness,
                                                          'y': community.cooperation_rate})
        chart_data['data']['datasets'][1]['data'].append({'x': community.positivity_of_gossip,
                                                          'y': community.cooperation_rate})
    return chart_data


def get_gen_length_vs_cooperation_rate_chart_data():
    """
    Get data for the chart on the historical reputation game data page that compares the length of generations in a
    community to the cooperation rate of that community
    :return: The chart data
    """
    communities = ReputationCommunity.query.filter_by(corrupted_observations=False, simulated=True).all()
    chart_data = {'type': 'line',
                 'data': {'labels': [],
                          'datasets': [
                     {
                         'label': "Length of generations of the community vs cooperation rate of community",
                         'data': [],
                         'backgroundColor': "#2ECC40", 'borderColor': "#0074D9", 'fill': False
                     },
                     {
                         'label': "Length of generation vs cooperation rate of generation",
                         'data': [],
                         'backgroundColor': "#FF4136", 'borderColor': "#0074D9", 'fill': False
                     }
                 ]},
                 'options': {
                     'title': {'display': True,
                               'text': "A scatter of the length of the generation vs "
                                       "the cooperation rate of each generation"},
                     'scales': {'yAxes': [{'ticks': {'max': 100, 'min': 0},
                                           'scaleLabel': {'display': True, 'labelString': "Cooperation Rate"}}],
                                'xAxes': [{'scaleLabel': {'display': True, 'labelString': "Generation Length"}}]}
                 }
                 }
    community_gen_length_cooperation_rate_count = {}
    generation_gen_length_cooperation_rate_count = {}
    max_length = 0
    for community in communities:
        if community.length_of_generations > max_length:
            max_length = community.length_of_generations
        if community.length_of_generations not in community_gen_length_cooperation_rate_count:
            community_gen_length_cooperation_rate_count[community.length_of_generations] =\
                {'coop_rate_sum': community.cooperation_rate, 'community_count': 1}
        else:
            community_gen_length_cooperation_rate_count[community.length_of_generations]['coop_rate_sum'] += \
                community.cooperation_rate
            community_gen_length_cooperation_rate_count[community.length_of_generations]['community_count'] += 1
        for gen in community.generations:
            if gen.get_length() > max_length:
                max_length = gen.get_length()
            if gen.get_length() not in generation_gen_length_cooperation_rate_count:
                generation_gen_length_cooperation_rate_count[gen.get_length()] = \
                    {'coop_rate_sum': gen.cooperation_rate, 'gen_count': 1}
            else:
                generation_gen_length_cooperation_rate_count[gen.get_length()][
                    'coop_rate_sum'] += gen.cooperation_rate
                generation_gen_length_cooperation_rate_count[gen.get_length()]['gen_count'] += 1
    for i in range(0, max_length):
        if i in community_gen_length_cooperation_rate_count and i in generation_gen_length_cooperation_rate_count:
            chart_data['data']['labels'].append(i)
        else:
            continue
        if i in community_gen_length_cooperation_rate_count:
            chart_data['data']['datasets'][0]['data'].append(
                community_gen_length_cooperation_rate_count[i]['coop_rate_sum'] /
                community_gen_length_cooperation_rate_count[i]['community_count'])
        else:
            chart_data['data']['datasets'][0]['data'].append(None)
        if i in generation_gen_length_cooperation_rate_count:
            chart_data['data']['datasets'][1]['data'].append(
                generation_gen_length_cooperation_rate_count[i]['coop_rate_sum'] /
                generation_gen_length_cooperation_rate_count[i]['gen_count'])
        else:
            chart_data['data']['datasets'][1]['data'].append(None)
    return chart_data


def get_cooperation_rate_vs_social_welfare_chart_data():
    """
    Get the chart data that compares the social welfare of a community to the cooperation rate of that community
    :return: the chart data
    """
    communities = ReputationCommunity.query.filter_by(corrupted_observations=False, simulated=True).all()
    chart_data = {'type': 'scatter',
                  'data': {'datasets': [
                      {
                          'label': "Cooperation rate of community vs social welfare of community",
                          'data': [],
                          'backgroundColor': "#3D9970", 'borderColor': "#0074D9"
                      },
                      {
                          'label': "Cooperation rate of generation vs social welfare of generation",
                          'data': [],
                          'backgroundColor': "#FF851B", 'borderColor': "#0074D9"
                      }
                  ]},
                  'options': {
                      'title': {'display': True,
                                'text': "A scatter of the cooperation rate in comparison to the social welfare"},
                      'scales': {'xAxes': [{'ticks': {'max': 100, 'min': 0},
                                            'scaleLabel': {'display': True, 'labelString': "Cooperation Rate"}}],
                                 'yAxes': [{'scaleLabel': {'display': True, 'labelString': "Social Welfare"}}]}
                  }
                  }
    for community in communities:
        chart_data['data']['datasets'][0]['data'].append({'x': community.cooperation_rate, 'y': community.fitness})
        for generation in community.generations:
            chart_data['data']['datasets'][1]['data'].append({'x': generation.cooperation_rate, 'y': generation.fitness})
    return chart_data


def get_strategies_vs_cooperation_rate_chart_data():
    """
    Get the chart data for the reputation game historical data page chart that concerns itself with comparing the
    concentration of each strategy in each community to the cooperation rate of that community
    :return: The chart data
    """
    # A dataset for each strategy, a datapoint for each generation they are in, x is the count of the strategy in that
    # generation, y is the cooperation rate of that generation
    communities = ReputationCommunity.query.filter_by(corrupted_observations=False, simulated=True).all()
    chart_data = {'type': 'line',
                  'data': {'datasets': []},
                  'options': {
                      'title': {'display': True,
                                'text': "A line chart comparing the counts of strategies in"
                                        " generations and the avergae of those generations cooperation rate"},
                      'scales': {'xAxes': [{'scaleLabel': {'display': True, 'labelString': "Strategy Count"}}],
                                 'yAxes': [{'ticks': {'max': 100, 'min': 0},
                                            'scaleLabel': {'display': True, 'labelString': "Cooperation Rate"}}]},
                      'aspectRatio': 1,
                      'legend': {'position': 'bottom'}
                  }
                  }
    created_datasets = {}
    hex_digits = list("0123456789ABCDEF")
    max_strat_count = 0
    for community in communities:
        for generation in community.generations:
            strategy_counts = db.session.query(ReputationPlayer.strategy, func.count('*')).\
                filter_by(community_id=community.id, generation_id=generation.id).\
                group_by(ReputationPlayer.strategy).all()
            for player_strat in strategy_counts:
                strategy = ReputationStrategy.query.filter_by(id=player_strat[0]).first()
                if player_strat[1] > max_strat_count:
                    max_strat_count = player_strat[1]
                if strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +\
                        strategy.trust_model + " " + strategy.options in created_datasets:
                    if player_strat[1] in created_datasets[strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                                           strategy.trust_model + " " + strategy.options]:
                        created_datasets[strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                         strategy.trust_model + " " + strategy.options][player_strat[1]]['coop_rate_sum'] += \
                            generation.cooperation_rate
                        created_datasets[strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                         strategy.trust_model + " " + strategy.options][player_strat[1]]['gen_count'] += 1
                    else:
                        created_datasets[strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                         strategy.trust_model + " " + strategy.options][player_strat[1]] =\
                            {'coop_rate_sum': generation.cooperation_rate, 'gen_count': 1}
                else:
                    created_datasets[strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                     strategy.trust_model + " " + strategy.options] = \
                        {player_strat[1]: {'coop_rate_sum': generation.cooperation_rate, 'gen_count': 1}}
    chart_data['data']['labels'] = [i for i in range(max_strat_count)]
    for dataset in created_datasets:
        hex_colour = "#" + ''.join([hex_digits[random.randint(0, len(hex_digits) - 1)] for _ in range(6)])
        chart_data['data']['datasets'].append({'label': dataset, 'data': [],
                                               'borderColor': hex_colour, 'backgroundColor': hex_colour, 'fill': False})
        index = len(chart_data['data']['datasets'])-1
        for i in range(max_strat_count):
            if i in created_datasets[dataset]:
                chart_data['data']['datasets'][index]['data'].append(created_datasets[dataset][i]['coop_rate_sum'] /
                                                                     created_datasets[dataset][i]['gen_count'])
            else:
                chart_data['data']['datasets'][index]['data'].append(None)
    return chart_data

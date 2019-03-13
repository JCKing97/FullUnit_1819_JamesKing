"""routes.py: A group of handlers for routes relating to indirect reciprocity"""

from flask import render_template, url_for, request, jsonify, current_app
from app.indir_rec import bp
import requests
from ..models import ReputationCommunity, ReputationGeneration, ReputationPlayer, ReputationStrategy
from app import db
import random
from sqlalchemy.sql import func
from flask_login import current_user
from rq.job import Job
from typing import Dict, List
from .action_logic import ActionType
import pprint


@bp.route('/reputation', methods=['GET', 'POST'])
def reputation():
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    if request.method == 'GET':
        return render_template('reputation.html', title='Reputation', strategies=strategies)
    if request.method == 'POST':
        form_data = request.get_json()
        strategy_counts = form_data['strategy_counts']
        player_count = 0
        for strategy in strategy_counts:
            player_count += strategy['count']
        if 4 < player_count < 200 and 0 <= int(form_data['num_of_onlookers']) \
                and 2 <= int(form_data['num_of_generations']) <= 20 \
                and 5 <= int(form_data['length_of_generations']) <= 200 \
                and 0 <= float(form_data['mutation_chance']) <= 1:
            community = ReputationCommunity(simulated=False, timed_out=False)
            db.session.add(community)
            db.session.commit()
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
            return jsonify({'url': url_for('indir_rec.reputation_finished', reputation_id=community.id,
                                           job_id=job.get_id())})
        return render_template('reputation.html', title='Reputation', strategies=strategies)


@bp.route('/is_reputation_finished/<reputation_id>/<job_id>')
def is_reputation_finished(reputation_id, job_id):
    """The route to ping to check whether a reputation game has been finished or not"""
    community = ReputationCommunity.query.filter_by(id=reputation_id).first_or_404()
    if community.is_finished():
        finished = True
    elif Job(job_id, current_app.redis).is_failed:
        community.timed_out = True
        db.session.commit()
        finished = True
    else:
        finished = False
    return jsonify({'finished': finished,
                    'url': url_for('indir_rec.reputation_finished', reputation_id=reputation_id, job_id=job_id)})


@bp.route('/reputation_finished/<reputation_id>/', defaults={'job_id': None})
@bp.route('/reputation_finished/<reputation_id>/<job_id>')
def reputation_finished(reputation_id, job_id):
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    community: ReputationCommunity = ReputationCommunity.query.filter_by(id=reputation_id).first_or_404()
    if community.timed_out:
        return render_template('reputation_timed_out.html', title='Reputation Timed Out', reputation_id=reputation_id)
    if community.is_finished():
        population_chart_data, strategy_colours = get_population_chart_data_and_strategy_colours(community)
        measurement_chart_data = get_measurements_chart_data(community)
        community_fitness_stats = db.session.query(func.max(ReputationCommunity.fitness).label("max_fit"),
                                                   func.min(ReputationCommunity.fitness).label("min_fit"),
                                                   func.avg(ReputationCommunity.fitness).label("avg_fit")).one()
        timepoints = [timepoint for timepoint in range(community.length_of_generations)]
        players = get_players(community)
        num_of_players_per_gen = len(players[0])
        actions = get_actions(community)
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
        if Job(job_id, current_app.redis).is_failed:
            community.timed_out = True
            db.session.commit()
            return render_template('reputation_timed_out.html', title='Reputation Timed Out', reputation_id=reputation_id)
        else:
            return render_template('reputation_running.html', title='Reputation Running', reputation_id=reputation_id,
                                   job_id=job_id)
    else:
        return render_template('reputation_running.html', title='Reputation Running', reputation_id=reputation_id,
                               job_id=job_id)


def get_players(community: ReputationCommunity):
    players: Dict[int, Dict[int, Dict[str, int]]] = {}
    for generation in community.generations:
        gen_players: Dict[int, Dict[str, int]] = {}
        for player in generation.players:
            gen_players[player.player_id] = {'cooperation_rate': player.cooperation_rate,
                                             'social_activeness': player.social_activeness,
                                             'positivity_of_gossip': player.positivity_of_gossip,
                                             'fitness': player.fitness, 'strategy': player.strategy}
        players[generation.generation_id] = gen_players
    return players


def get_actions(community: ReputationCommunity):
    actions: Dict[int, Dict[int, List[Dict]]] = {}
    for generation in community.generations:
        gen_actions: Dict[int, List[Dict]] = {}
        for player in generation.players:
            for action in player.actions:
                if action.type is ActionType.INTERACTION:
                    if action.timepoint % community.length_of_generations in gen_actions:
                        gen_actions[action.timepoint % community.length_of_generations].append({'type': 'interaction',
                                                              'donor': action.get_donor_player_id(),
                                                              'recipient': action.get_recipient_player_id(),
                                                              'action': str(action.action)})
                    else:
                        gen_actions[action.timepoint % community.length_of_generations] = [{'type': 'interaction', 'donor': action.get_donor_player_id(),
                                                         'recipient': action.get_recipient_player_id(),
                                                          'action': str(action.action)}]
                elif action.type is ActionType.GOSSIP:
                    if action.timepoint % community.length_of_generations in gen_actions:
                        gen_actions[action.timepoint % community.length_of_generations].append({'type': 'gossip',
                                                              'gossiper': action.get_gossiper_player_id(),
                                                              'about': action.get_about_player_id(),
                                                              'recipient': action.get_recipient_player_id(),
                                                              'gossip': str(action.gossip)})
                    else:
                        gen_actions[action.timepoint % community.length_of_generations] = [{'type': 'gossip', 'gossiper': action.get_gossiper_player_id(),
                                                          'about': action.get_about_player_id(),
                                                          'recipient': action.get_recipient_player_id(),
                                                          'gossip': str(action.gossip)}]
        actions[generation.generation_id] = gen_actions
    return actions


def get_population_chart_data_and_strategy_colours(community: ReputationCommunity):
    chart_data = {'type': 'line', 'data': {'datasets': [], 'labels': []},
                  'options': {'title': {'display': True,'text': "Population fluctuation across the generations"},
                              'scales': {'yAxes': [{'scaleLabel': {'display': True, 'labelString': "Strategy Count"}}],
                                         'xAxes': [{'scaleLabel': {'display': True, 'labelString': "Generation"}}]}}}
    strategy_colours: Dict[int, str] = {}
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
                hex_colour = "#" + ''.join([hex_digits[random.randint(0, len(hex_digits) - 1)] for _ in range(6)])
                chart_data['data']['datasets']. \
                    append({'label': strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +
                                     strategy.trust_model + " " + strategy.options,
                            'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
                            'borderColor': hex_colour, 'backgroundColor': hex_colour})
                chart_data['data']['datasets'][-1]['data'][gen.generation_id] += 1
                strategy_colours[player.strategy] = hex_colour
            else:
                for dataset in chart_data['data']['datasets']:
                    if dataset['label'] == strategy.donor_strategy + " " + strategy.non_donor_strategy + " " +\
                            strategy.trust_model + " " + strategy.options:
                        dataset['data'][gen.generation_id] += 1
                        break
    return chart_data, strategy_colours


def get_measurements_chart_data(community: ReputationCommunity):
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

"""routes.py: A group of handlers for routes relating to indirect reciprocity"""

from flask import render_template, url_for, request, jsonify, current_app
from app.indir_rec import bp
import requests
from ..models import ReputationCommunity, ReputationGeneration
from app import db
from .action_logic import ActionType
import random
from sqlalchemy.sql import func


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
            community = ReputationCommunity(simulated=False)
            db.session.add(community)
            db.session.commit()
            current_app.task_queue.enqueue('app.indir_rec.run_game.reputation_run', strategy_counts,
                                           int(form_data['num_of_onlookers']), int(form_data['num_of_generations']),
                                           int(form_data['length_of_generations']), float(form_data['mutation_chance']),
                                           community.id)
            return jsonify({'url': url_for('indir_rec.reputation_finished', reputation_id=community.id)})
        return render_template('reputation.html', title='Reputation', strategies=strategies)


@bp.route('/is_reputation_finished/<reputation_id>')
def is_reputation_finished(reputation_id):
    """The route to ping to check whether a reputation game has been finished or not"""
    community = ReputationCommunity.query.filter_by(id=reputation_id).first_or_404()
    return jsonify({'finished': community.is_finished(),
                    'url': url_for('indir_rec.reputation_finished', reputation_id=reputation_id)})


@bp.route('/reputation_finished/<reputation_id>')
def reputation_finished(reputation_id):
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    community: ReputationCommunity = ReputationCommunity.query.filter_by(id=reputation_id).first_or_404()
    if community.is_finished():
        population_chart_data = get_population_chart_data(community)
        measurement_chart_data = get_measurements_chart_data(community)
        community_fitness_stats = db.session.query(func.max(ReputationCommunity.fitness).label("max_fit"),
                                                   func.min(ReputationCommunity.fitness).label("min_fit"),
                                                   func.avg(ReputationCommunity.fitness).label("avg_fit")).one()
        timepoints = [timepoint for timepoint in range(community.length_of_generations)]
        return render_template('reputation_finished.html', title='Reputation Finished', strategies=strategies,
                               community=community, action_type=ActionType, population_chart_data=population_chart_data,
                               measurement_chart_data=measurement_chart_data,
                               fitness_chart_data=get_fitness_chart_data(community),
                               lowest_fitness=community_fitness_stats.min_fit,
                               highest_fitness=community_fitness_stats.max_fit,
                               average_fitness=round(community_fitness_stats.avg_fit), timepoints=timepoints)
    else:
        return render_template('reputation_running.html', title='Reputation Running', reputation_id=reputation_id)


def get_population_chart_data(community: ReputationCommunity):
    chart_data = {'type': 'line', 'data': {'datasets': [], 'labels': []},
                  'options': {'title': {'display': True,'text': "Population fluctuation across the generations"},
                              'scales': {'yAxes': [{'scaleLabel': {'display': True,'labelString': "Strategy Count"}}],
                                         'xAxes': [{'scaleLabel': {'display': True,'labelString': "Generation"}}]}}}
    created_datasets = []
    hex_digits = list("0123456789ABCDEF")
    for generation in community.generations:
        gen: ReputationGeneration = generation
        chart_data['data']['labels'].append(gen.id)
        for player in generation.players:
            if player.strategy_name + " " + player.strategy_options not in created_datasets:
                created_datasets.append(player.strategy_name + " " + player.strategy_options)
                hex_colour = "#" + ''.join([hex_digits[random.randint(0, len(hex_digits) - 1)] for _ in range(6)])
                chart_data['data']['datasets']. \
                    append({'label': player.strategy_name + " " + player.strategy_options,
                            'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
                            'borderColor': hex_colour, 'backgroundColor': hex_colour})
                chart_data['data']['datasets'][-1]['data'][gen.id] += 1
            else:
                for dataset in chart_data['data']['datasets']:
                    if dataset['label'] == player.strategy_name + " " + player.strategy_options:
                        dataset['data'][gen.id] += 1
                        break
    return chart_data


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
        chart_data['data']['labels'].append(gen.id)
        for dataset in chart_data['data']['datasets']:
            if dataset['label'] == "Cooperation rate":
                dataset['data'][gen.id] = gen.cooperation_rate
            elif dataset['label'] == "Social activeness":
                dataset['data'][gen.id] = gen.social_activeness
            elif dataset['label'] == "Positivity of gossip":
                dataset['data'][gen.id] = gen.positivity_of_gossip
    return chart_data


def get_fitness_chart_data(community: ReputationCommunity):
    return {'type': 'bar', 'data': {'labels': [gen.id for gen in community.generations],
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
    return render_template('reputation_historical.html', title='Reputation Games Historical Data', strategies=strategies,
                           social_vs_cooperation_rate_chart_data=social_vs_cooperation_rate_chart_data,
                           gen_length_vs_cooperation_rate_chart_data=gen_length_vs_cooperation_rate_chart_data,
                           cooperation_rate_vs_social_welfare_chart_data=cooperation_rate_vs_social_welfare_chart_data)


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
    chart_data = {'type': 'scatter',
                 'data': {'datasets': [
                     {
                         'label': "Length of generations of the community vs cooperation rate of community",
                         'data': [],
                         'backgroundColor': "#2ECC40", 'borderColor': "#0074D9"
                     },
                     {
                         'label': "Length of generation vs cooperation rate of generation",
                         'data': [],
                         'backgroundColor': "#FF4136", 'borderColor': "#0074D9"
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
    for community in communities:
        chart_data['data']['datasets'][0]['data'].append({'x': community.length_of_generations,
                                                          'y': community.cooperation_rate})
        for gen in community.generations:
            chart_data['data']['datasets'][1]['data'].append({'x': gen.get_length(),
                                                              'y': gen.cooperation_rate})
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

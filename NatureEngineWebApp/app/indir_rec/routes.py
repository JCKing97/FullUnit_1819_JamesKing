"""routes.py: A group of handlers for routes relating to indirect reciprocity"""

from flask import render_template, url_for, request, jsonify, current_app
from app.indir_rec import bp
import requests
from ..models import ReputationCommunity, ReputationGeneration, ReputationStrategy, ReputationPlayer
from app import db
from .action_logic import ActionType
from sqlalchemy import func
import random


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
        return render_template('reputation_finished.html', title='Reputation Finished', strategies=strategies,
                               community=community, action_type=ActionType, population_chart_data=population_chart_data)
    else:
        return render_template('reputation_running.html', title='Reputation Running', reputation_id=reputation_id)


def get_population_chart_data(community: ReputationCommunity):
    chart_data = {'type': 'bar', 'data': {'datasets': [], 'labels': []},
                       'options': {'title': {'display': True,
                                             'text': "Population fluctuation across the generations in"
                                                     " comparison to the cooperation rate"},
                                   'scales': {'yAxes': [{'id': 'population-y-axis',
                                                         'scaleLabel': {'display': True,
                                                                        'labelString': "Strategy Count"},
                                                         'position': 'left'},
                                                        {'id': 'coop-y-axis',
                                                         'scaleLabel': {'display': True,
                                                                        'labelString': "Cooperation Rate"},
                                                         'position': 'right'}
                                                        ],
                                              'xAxes': [{'scaleLabel': {'display': True,
                                                                        'labelString': "Generation"}}]}}}
    cooperation_social_chart_dataset = {'type': 'bar', 'label': 'Cooperation Rate',
                                        'data': [], 'yAxisID': 'coop-y-axis'}
    created_datasets = []
    hex_digits = list("0123456789ABCDEF")
    for generation in community.generations:
        gen: ReputationGeneration = generation
        chart_data['data']['labels'].append(gen.id)
        cooperation_social_chart_dataset['data'].append({'x': generation.id,
                                                         'y': generation.cooperation_rate})
        for player in generation.players:
            if player.strategy_name + " " + player.strategy_options not in created_datasets:
                created_datasets.append(player.strategy_name + " " + player.strategy_options)
                chart_data['data']['datasets']. \
                    append({'label': player.strategy_name + " " + player.strategy_options,
                            'data': [0 for _ in range(len(community.generations.all()))], 'fill': False,
                            'borderColor': "#" + ''.join([hex_digits[random.randint(0, len(hex_digits) - 1)]
                                                          for _ in range(6)]),
                            'type': 'line', 'yAxisID': 'population-y-axis'})
                chart_data['data']['datasets'][-1]['data'][gen.id] += 1
            else:
                for dataset in chart_data['data']['datasets']:
                    if dataset['label'] == player.strategy_name + " " + player.strategy_options:
                        dataset['data'][gen.id] += 1
                        break
    chart_data['data']['datasets'].append(cooperation_social_chart_dataset)
    return chart_data

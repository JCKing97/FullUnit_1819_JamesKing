"""routes.py: A group of handlers for routes relating to indirect reciprocity"""

from flask import render_template, url_for, request, jsonify, current_app
from app.indir_rec import bp
import requests


@bp.route('/reputation', methods=['GET', 'POST'])
def reputation():
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    if request.method == 'GET':
        return render_template('reputation.html', title='Reputation', strategies=strategies)
    if request.method == 'POST':
        strategy_counts = request.get_json()['strategy_counts']
        players = []
        for strategy in strategy_counts:
            for i in range(0, strategy['count']):
                players.append(strategy['name'])
        if 2 < len(players) < 50:
            return jsonify({'url': url_for('indir_rec.reputation_finished')})
        return render_template('reputation.html', title='Reputation', strategies=strategies)


@bp.route('/reputation_finished')
def reputation_finished():
    response = requests.get(current_app.config['AGENTS_URL'] + "strategy")
    strategies = response.json()['strategies']
    return render_template('reputation_finished.html', title='Reputation Finished', strategies=strategies)

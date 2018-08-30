from flask import render_template, redirect, url_for
from app import app
from flask_babel import _
from app.forms import GameSelectAgentsForm
from app.game_analysis import get_points
from app.models import Strategy


@app.route('/')
@app.route('/index')
def index():
    return render_template('index.html', title='Home')


@app.route('/game', methods=['POST', 'GET'])
def game():
    strategies = Strategy.query.all()
    form = GameSelectAgentsForm()
    edit_agents(form, strategies)
    if form.validate_on_submit():
        agents = {'agent1': form.strats_field1.data, 'agent2': form.strats_field2.data}
        return redirect(url_for('game_finished', game_id=1))
    return render_template('game.html', title='Game', form=form, strategies=strategies)


def edit_agents(form, strategies):
    form.strats_field1.choices = [(strat.name, strat.name) for strat in strategies]
    form.strats_field2.choices = [(strat.name, strat.name) for strat in strategies]


@app.route('/game_finished/<game_id>')
def game_finished(game_id):
    interaction_history = [{'agent1': False, 'agent2': True}, {'agent1': False, 'agent2': True},
                           {'agent1': False, 'agent2': False}, {'agent1': False, 'agent2': False},
                           {'agent1': True, 'agent2': True}, {'agent1': True, 'agent2': True}]
    agent_points = get_points(interaction_history)
    strategies = Strategy.query.all()
    return render_template('game_finished.html', title='Game Finished', game_id=game_id,
                           interaction_history=interaction_history, agent_points=agent_points,
                           strategies=strategies)


@app.route('/tournament')
def tournament():
    return render_template('tournament.html', title='Tournament')


@app.route('/communities')
def communities():
    return render_template('communities.html', title='Communities')


@app.route('/about')
def about():
    return render_template('about.html', title='About')

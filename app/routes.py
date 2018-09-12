from flask import render_template, redirect, url_for, send_file
from app import app
from app.forms import GameSelectAgentsForm
from app.analysis import get_game_points, get_tournament_points
from app.models import Strategy, Game, Tournament


reports = ["Strategy Theory", "Strategy Analysis", "Tournaments and Communities", "Tournament Development Methodology"]

@app.route('/')
@app.route('/index')
def index():
    """The template to render for the home page"""
    return render_template('index.html', title='Home', reports=reports)


@app.route('/game', methods=['POST', 'GET'])
def game():
    """Handles rendering the template with the form to select
    and start a game and also the logic when this form is posted"""
    strategies = Strategy.query.all()
    form = GameSelectAgentsForm()
    edit_agents(form, strategies)
    if form.validate_on_submit():
        return redirect(url_for('game_finished', game_id=1))
    return render_template('game.html', title='Game', form=form, strategies=strategies, reports=reports)


def edit_agents(form, strategies):
    """Creates the choices for the strategy selection fields for an agent"""
    form.strats_field1.choices = [(strat.name, strat.name) for strat in strategies]
    form.strats_field2.choices = [(strat.name, strat.name) for strat in strategies]


@app.route('/game_finished/<game_id>')
def game_finished(game_id):
    """Displays the information of a finished game with the game_id provided"""
    interaction_history = Game.query.filter_by(id=game_id).first_or_404().get_interaction_history()
    agent_points = get_game_points(interaction_history)
    agents = Game.query.filter_by(id=game_id).first().agents
    strategy_names = [agent.strategy_name for agent in agents]
    strategies = Strategy.query.filter((Strategy.name==strategy_names[0]) | (Strategy.name==strategy_names[1])).all()
    return render_template('game_finished.html', title='Game Finished', game_id=game_id,
                           interaction_history=interaction_history, agent_points=agent_points,
                           strategies=strategies, agents=agents, reports=reports)


@app.route('/tournament')
def tournament():
    return render_template('tournament.html', title='Tournament', reports=reports)


@app.route('/tournament_finished/<tournament_id>')
def tournament_finished(tournament_id):
    """Displays the information of a finished tournament with the tournament_id provided"""
    games = Tournament.query.filter_by(id=tournament_id).first_or_404().games.all()
    agent_points = get_tournament_points(games)
    agents = []
    for game in games:
        for agent in game.agents:
            agents.append(agent)
    strategy_names = [agent.strategy_name for agent in agents]
    strategies = []
    for name in strategy_names:
        strategies.append(Strategy.query.filter(Strategy.name==name).all())
    return render_template('tournament_finished.html', title='Tournament Finished', tournament_id=tournament_id,
                           agent_points=agent_points, strategies=strategies, agents=agents, game_length=4, games=games,
                           reports=reports)


@app.route('/communities')
def communities():
    return render_template('communities.html', title='Communities', reports=reports)


@app.route('/multiplayer')
def multiplayer():
    return render_template('multiplayer.html', title='Multiplayer', reports=reports)


@app.route('/about')
def about():
    return render_template('about.html', title='About', reports=reports)


@app.route('/report/<report_name>')
def report(report_name):
    return send_file('{}/static/docs/{}.pdf'.format(app.root_path, report_name),
                     mimetype="application/pdf",
                     as_attachment=True)
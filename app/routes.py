from app import app, db
from app.models import Match, Round, Player, Action
from app.forms import MatchSelectPlayersForm
from flask import render_template, redirect, url_for
import axelrod as axl
from app.axelrod_database_conversion import match_result_to_database
from app.analysis import get_match_points

@app.route('/')
@app.route('/index')
def index():
    """The template to render for the home page"""
    return render_template('index.html', title='Home')


@app.route('/match/<level>', methods=['POST', 'GET'])
def match(level):
    """Handles rendering the template with the form to select
    and start a match and also the logic when this form is posted"""
    if level == "Advanced":
        strategies = [s() for s in axl.strategies]
    else:
        strategies = [s() for s in axl.basic_strategies]
    print(strategies)
    form = MatchSelectPlayersForm()
    edit_players(form, strategies)
    if form.validate_on_submit():
        strat_dict = {strat.name: strat for strat in strategies}
        players = (strat_dict[form.strats_field1.data], strat_dict[form.strats_field2.data])
        result = axl.Match(players, turns=form.rounds.data).play()
        match_id = match_result_to_database(results=result, players=players)
        return redirect(url_for('match_run', match_id=match_id))
    return render_template('match.html', title='Match', form=form, strategies=strategies)

def edit_players(form, strategies):
    """Creates the choices for the strategy selection fields for a player"""
    form.strats_field1.choices = [(strat.name, strat.name) for strat in strategies]
    form.strats_field2.choices = [(strat.name, strat.name) for strat in strategies]


@app.route('/match_run/<match_id>')
def match_run(match_id):
    """Displays the information of a finished match with the match_id provided"""
    interaction_history = Match.query.filter_by(id=match_id).first_or_404().get_interaction_history()
    player_points = get_match_points(interaction_history)
    players = Match.query.filter_by(id=match_id).first().players
    strat_dict = {s().name: s() for s in axl.strategies}
    return render_template('match_run.html', title='Match Finished', match_id=match_id,
                           interaction_history=interaction_history, player_points=player_points,
                           strat_dict=strat_dict, players=players)


@app.route('/tournament/<level>')
def tournament(level):
    if level == "Advanced":
        strategies = [s() for s in axl.strategies]
    else:
        strategies = [s() for s in axl.basic_strategies]
    return render_template('tournament.html', title='Tournament', strategies=strategies)


@app.route('/tournament_finished/<tournament_id>')
def tournament_finished(tournament_id):
    """Displays the information of a finished tournament with the tournament_id provided"""
    matches = Tournament.query.filter_by(id=tournament_id).first_or_404().matches.all()
    player_points = get_tournament_points(matches)
    players = []
    for match in matches:
        for player in match.players:
            players.append(player)
    strategy_names = [player.strategy_name for player in players]
    strategies = []
    for name in strategy_names:
        strategies.append(Strategy.query.filter(Strategy.name==name).all())
    return render_template('tournament_finished.html', title='Tournament Finished', tournament_id=tournament_id,
                           player_points=player_points, strategies=strategies, players=players, match_length=4, matches=matches)

@app.route('/reputation')
def reputation():
    return render_template('reputation.html', title='Reputation')

@app.route('/about')
def about():
    return render_template('about.html', title='About')

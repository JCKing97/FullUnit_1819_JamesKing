"""A module containing the functionality for handling routes in the main blueprint (IPD, home and about pages).
This includes processing of the data sent to them, and processing of the data returned by them"""

from app import db
from app.main import bp
from app.models import Match, Tournament
from app.main.forms import MatchSelectPlayersForm
from flask import render_template, redirect, url_for, request, jsonify, current_app
import axelrod as axl
from app.main.axelrod_database_conversion import match_result_to_database
from app.main.analysis import get_match_points
from rq import get_current_job
from sqlalchemy import desc, asc

@bp.route('/')
@bp.route('/index')
def index():
    """The home page route"""
    return render_template('index.html', title='Home')


@bp.route('/match/<level>', methods=['POST', 'GET'])
def match(level):
    """Handles rendering the template with the form to select
    and start a match and also the logic when this form is posted
    :param level: The level of strategies which the user has selected"""
    if level == "Advanced":
        strategies = [{'id': axl.strategies.index(s), 'name': s.name} for s in axl.strategies]
    else:
        strategies = [{'id': axl.basic_strategies.index(s), 'name': s.name} for s in axl.basic_strategies]
    strat_dict = {s.name: s() for s in axl.strategies}
    form = MatchSelectPlayersForm()
    edit_players(form, strategies)
    if form.validate_on_submit():
        players = (strat_dict[form.strats_field1.data], strat_dict[form.strats_field2.data])
        result = axl.Match(players, turns=form.rounds.data).play()
        match_id = match_result_to_database(results=result, players=players)
        return redirect(url_for('main.match_run', match_id=match_id))
    return render_template('match.html', title='Match', form=form, strategies=strategies, strat_dict=strat_dict)

def edit_players(form, strategies):
    """Creates the choices for the strategy selection fields for a player"""
    form.strats_field1.choices = [(strat['name'], strat['name']) for strat in strategies]
    form.strats_field2.choices = [(strat['name'], strat['name']) for strat in strategies]


@bp.route('/match_run/<match_id>')
def match_run(match_id):
    """Displays the information of a finished match with the match_id provided
    :param match_id: The id of the match to display the information of"""
    interaction_history = Match.query.filter_by(id=match_id).first_or_404().get_interaction_history()
    player_points = get_match_points(interaction_history)
    players = Match.query.filter_by(id=match_id).first().players
    strat_dict = {s().name: s() for s in axl.strategies}
    strategies = []
    for player in players:
        if {'name': player.strategy} not in strategies:
            strategies.append({'name': player.strategy})
    print(strategies)
    return render_template('match_finished.html', title='Match Finished', match_id=match_id,
                           interaction_history=interaction_history, player_points=player_points,
                           strat_dict=strat_dict, players=players, strategies=strategies)


@bp.route('/tournament/<level>', methods=['GET', 'POST'])
def tournament(level):
    """Handles rendering the template with the form to select
        and start a tournament and also the logic when this form is posted
        :param level: The level of strategies the user wishes to received (basic or advanced)"""
    if level == "Advanced":
        strategies = [{'id': axl.strategies.index(s), 'name': s.name} for s in axl.strategies]
    else:
        strategies = [{'id': axl.basic_strategies.index(s), 'name': s.name} for s in axl.basic_strategies]
    strat_dict = {s().name: s() for s in axl.strategies}
    if request.method == 'GET':
        return render_template('tournament.html', level=level, title='Tournament', strategies=strategies, strat_dict=strat_dict)
    if request.method == 'POST':
        strategy_counts = request.get_json()['strategy_counts']
        players = []
        for strategy in strategy_counts:
            for i in range(0, strategy['count']):
                players.append(strat_dict[strategy['name']])
        if 2 < len(players) < 50:
            new_tournament = Tournament()
            db.session.add(new_tournament)
            db.session.commit()
            job = current_app.task_queue.enqueue('app.main.axelrod_database_conversion.tournament_run', players, new_tournament.id)
            return jsonify({'url': url_for('main.tournament_run', tournament_id=new_tournament.id, job_id=job.get_id())})
        else:
            return render_template('tournament.html', title='Tournament', level=level, strategies=strategies, strat_dict=strat_dict)


@bp.route('/tournament_run/<tournament_id>/<job_id>')
def tournament_run(tournament_id, job_id):
    """The functionality for when a tournament is running (rendering a template that pings the server to check)
    and also when the tournament has finished the logic for displaying an analysis of the tournament
    :param tournament_id: The id of the tournament that is being run or has finished
    :param job_id: The id of the redis job that this tournament used or is using"""
    this_tournament = Tournament.query.filter_by(id=tournament_id).first_or_404()
    players = this_tournament.players
    strat_dict = {s().name: s() for s in axl.strategies}
    strategies = []
    max_points = 0
    for player in players:
        if player.score > max_points:
            max_points = player.score
        if {'name': player.strategy} not in strategies:
            strategies.append({'name': player.strategy})
    top3_players = players.order_by(desc('score')).limit(3).all()
    top3_cooperative_players = players.order_by(desc('cooperation_rating')).limit(3).all()
    top3_defective_players = players.order_by(asc('cooperation_rating')).limit(3).all()
    if this_tournament.is_finished():
        return render_template('tournament_finished.html', title='Tournament', tournament_id=tournament_id,
                               players=players, strat_dict=strat_dict, max_points=max_points, top3_players=top3_players,
                               strategies=strategies, top3_cooperative_players=top3_cooperative_players,
                               top3_defective_players=top3_defective_players)
    else:
        return render_template('tournament_running.html', title='Tournament', tournament_id=tournament_id, job_id=job_id)


@bp.route('/is_tournament_finished/<tournament_id>/<job_id>')
def is_tournament_finished(tournament_id, job_id):
    """The route to ping to check whether a tournament has been finished or not"""
    this_tournament = Tournament.query.filter_by(id=tournament_id).first_or_404()
    print(get_current_job())
    print("Job id {}".format(job_id))
    return jsonify({'finished': this_tournament.is_finished(),
                    'url': url_for('main.tournament_run', tournament_id=tournament_id, job_id=job_id)})


@bp.route('/about')
def about():
    """The route for information about the website and the surrounding project"""
    return render_template('about.html', title='About')

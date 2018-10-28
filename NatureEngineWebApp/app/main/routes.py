from app import db
from app.main import bp
from app.models import Match, Tournament
from app.main.forms import MatchSelectPlayersForm
from flask import render_template, redirect, url_for, request, jsonify, current_app
import axelrod as axl
from app.main.axelrod_database_conversion import match_result_to_database
from app.main.analysis import get_match_points
from rq import get_current_job

@bp.route('/')
@bp.route('/index')
def index():
    """The template to render for the home page"""
    return render_template('index.html', title='Home')


@bp.route('/match/<level>', methods=['POST', 'GET'])
def match(level):
    """Handles rendering the template with the form to select
    and start a match and also the logic when this form is posted"""
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
    """Displays the information of a finished match with the match_id provided"""
    interaction_history = Match.query.filter_by(id=match_id).first_or_404().get_interaction_history()
    player_points = get_match_points(interaction_history)
    players = Match.query.filter_by(id=match_id).first().players
    strat_dict = {s().name: s() for s in axl.strategies}
    return render_template('match_run.html', title='Match Finished', match_id=match_id,
                           interaction_history=interaction_history, player_points=player_points,
                           strat_dict=strat_dict, players=players)


@bp.route('/tournament/<level>', methods=['GET', 'POST'])
def tournament(level):
    """Handles rendering the template with the form to select
        and start a tournament and also the logic when this form is posted"""
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
    """Displays the information of a running or finished tournament with the tournament_id provided"""
    this_tournament = Tournament.query.filter_by(id=tournament_id).first_or_404()
    if this_tournament.is_finished():
        return render_template('tournament_finished.html', title='Tournament')
    else:
        return render_template('tournament_running.html', title='Tournament', tournament_id=tournament_id, job_id=job_id)


@bp.route('/is_tournament_finished/<tournament_id>/<job_id>')
def is_tournament_finished(tournament_id, job_id):
    this_tournament = Tournament.query.filter_by(id=tournament_id).first_or_404()
    print(get_current_job())
    print("Job id {}".format(job_id))
    return jsonify({'finished': this_tournament.is_finished(),
                    'url': url_for('main.tournament_run', tournament_id=tournament_id, job_id=job_id)})


@bp.route('/reputation')
def reputation():
    return render_template('reputation.html', title='Reputation')


@bp.route('/about')
def about():
    return render_template('about.html', title='About')

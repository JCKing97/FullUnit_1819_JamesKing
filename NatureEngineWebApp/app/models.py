"""models.py: A module containing the objects for SQLAlchemy to map to a relational schema"""

__author__ = "James King adapted from Miguel Grinberg"

from app import db
from datetime import datetime


class Match(db.Model):
    """The database model of an IPD match"""
    id = db.Column(db.Integer, primary_key=True)
    timestamp = db.Column(db.DateTime, index=True, default=datetime.utcnow())
    rounds = db.relationship('Round', backref='match', lazy='dynamic')
    players = db.relationship('Player', backref='match', lazy='dynamic')
    actions = db.relationship('Action', backref='match', lazy='dynamic')

    def __repr__(self):
        return "<Match {}>".format(self.id)

    def get_interaction_history(self):
        """Get the interaction history of the match"""
        interaction_history = Action.query.filter(Action.match_id == self.id)
        return interaction_history.order_by(Action.round_num.asc()).all()


class Round(db.Model):
    """A database representation of a round of the Iterated Prisoner's Dilemma"""
    num = db.Column(db.Integer, primary_key=True)
    match_id = db.Column(db.Integer, db.ForeignKey('match.id'), primary_key=True)
    actions = db.relationship('Action', backref='round', lazy='dynamic')

    def __repr__(self):
        return '<Round {}, Match {}>'.format(self.num, self.match_id)


class Player(db.Model):
    """The database representation of an Iterated Prisoner's Dilemma player for a match"""
    id = db.Column(db.Integer, primary_key=True)
    match_id = db.Column(db.Integer, db.ForeignKey('match.id'), primary_key=True)
    strategy = db.Column(db.String(120))
    actions = db.relationship('Action', backref='player', lazy='dynamic')

    def __repr__(self):
        return '<Player {}, Strategy {}, Match {}>'.format(self.id, self.strategy, self.match_id)


class Action(db.Model):
    """The database representation of ann action by a player in a specific round towards another player
     (cooperate or defect)"""
    round_num = db.Column(db.Integer, db.ForeignKey('round.num'), primary_key=True)
    match_id = db.Column(db.Integer, db.ForeignKey('match.id'), primary_key=True)
    player_id = db.Column(db.Integer, db.ForeignKey('player.id'), primary_key=True)
    cooperate = db.Column(db.Boolean)

    def __repr__(self):
        return '<Cooperated? {}, Player {}, Round {}, Match {}>'.format(self.cooperate, self.player_id, self.round_num, self.match_id)


class Tournament(db.Model):
    """The database representation of an IPD tournament"""
    id = db.Column(db.Integer, primary_key=True)
    completed = db.Column(db.Boolean, default=False)
    error = db.Column(db.Boolean, default=False)
    players = db.relationship('TournamentPlayer', backref='tournament', lazy='dynamic')

    def is_finished(self):
        return self.completed


class TournamentPlayer(db.Model):
    """The database representation of a  player that has been part of a tournament"""
    id = db.Column(db.Integer, primary_key=True)
    tournament_id = db.Column(db.Integer, db.ForeignKey('tournament.id'), primary_key=True)
    strategy = db.Column(db.String(120))
    score = db.Column(db.Integer)
    rank = db.Column(db.Integer)
    cooperation_rating = db.Column(db.Float)
    wins = db.Column(db.Integer)

"""models.py: A module containing the objects for SQLAlchemy to map to a relational schema"""

__author__ = "James King adapted from Miguel Grinberg"

from app import db
from datetime import datetime


class Match(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    timestamp = db.Column(db.DateTime, index=True, default=datetime.utcnow())
    rounds = db.relationship('Round', backref='match', lazy='dynamic')
    players = db.relationship('Player', backref='match', lazy='dynamic')
    actions = db.relationship('Action', backref='match', lazy='dynamic')

    def __repr__(self):
        return "<Match {}>".format(self.id)

    def get_interaction_history(self):
        interaction_history = Action.query.filter(Action.match_id == self.id)
        return interaction_history.order_by(Action.round_num.asc()).all()


class Round(db.Model):
    num = db.Column(db.Integer, primary_key=True)
    match_id = db.Column(db.Integer, db.ForeignKey('match.id'), primary_key=True)
    actions = db.relationship('Action', backref='round', lazy='dynamic')

    def __repr__(self):
        return '<Round {}, Match {}>'.format(self.num, self.match_id)


class Player(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    match_id = db.Column(db.Integer, db.ForeignKey('match.id'), primary_key=True)
    strategy = db.Column(db.String(120))
    actions = db.relationship('Action', backref='player', lazy='dynamic')

    def __repr__(self):
        return '<Player {}, Strategy {}, Match {}>'.format(self.id, self.strategy, self.match_id)


class Action(db.Model):
    round_num = db.Column(db.Integer, db.ForeignKey('round.num'), primary_key=True)
    match_id = db.Column(db.Integer, db.ForeignKey('match.id'), primary_key=True)
    player_id = db.Column(db.Integer, db.ForeignKey('player.id'), primary_key=True)
    cooperate = db.Column(db.Boolean)

    def __repr__(self):
        return '<Cooperated? {}, Player {}, Round {}, Match {}>'.format(self.cooperate, self.player_id, self.round_num, self.match_id)


class Tournament(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    completed = db.Column(db.Boolean, default=False)

    def is_finished(self):
        return self.completed

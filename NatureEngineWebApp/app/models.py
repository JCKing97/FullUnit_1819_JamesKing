"""models.py: A module containing the objects for SQLAlchemy to map to a relational schema"""

__author__ = "James King adapted from Miguel Grinberg"

from app import db, login
from datetime import datetime
from typing import List
from werkzeug.security import generate_password_hash, check_password_hash
from flask_login import UserMixin
from app.search import add_to_index, remove_from_index, query_index


class User(UserMixin, db.Model):
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(64), index=True, unique=True)
    email = db.Column(db.String(120), index=True, unique=True)
    password_hash = db.Column(db.String(128))
    experiments = db.relationship('Experiment', backref='user', lazy='dynamic')

    def __repr__(self):
        return '<User {}>'.format(self.username)

    def set_password(self, password):
        self.password_hash = generate_password_hash(password)

    def check_password(self, password):
        return check_password_hash(self.password_hash, password)


@login.user_loader
def load_user(id):
    return User.query.get(int(id))


class SearchableMixin:

    @classmethod
    def search(cls, expression, page, per_page):
        ids, total = query_index(cls.__tablename__, expression, page, per_page)
        if total == 0:
            return cls.query.filter_by(id=0), 0
        when = []
        for i in range(len(ids)):
            when.append((ids[i], i))
        return cls.query.filter(cls.id.in_(ids)).order_by(
            db.case(when, value=cls.id)), total

    @classmethod
    def before_commit(cls, session):
        session._changes = {
            'add': list(session.new),
            'update': list(session.dirty),
            'delete': list(session.deleted)
        }

    @classmethod
    def after_commit(cls, session):
        print(session._changes['add'])
        for obj in session._changes['add']:
            if isinstance(obj, SearchableMixin):
                add_to_index(obj.__tablename__, obj)
        for obj in session._changes['update']:
            if isinstance(obj, SearchableMixin):
                add_to_index(obj.__tablename__, obj)
        for obj in session._changes['delete']:
            if isinstance(obj, SearchableMixin):
                remove_from_index(obj.__tablename__, obj)
        session._changes = None

    @classmethod
    def reindex(cls):
        for obj in cls.query:
            add_to_index(cls.__tablename__, obj)


db.event.listen(db.session, 'before_commit', SearchableMixin.before_commit)
db.event.listen(db.session, 'after_commit', SearchableMixin.after_commit)


class Experiment(SearchableMixin, db.Model):
    __searchable__ = ['label']
    id = db.Column(db.Integer, primary_key=True)
    community_id = db.Column(db.Integer, db.ForeignKey('reputation_community.id'))
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    label = db.Column(db.String(128))


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


class ReputationCommunity(db.Model):
    """The community which a game of indirect reciprocity is run on"""
    id = db.Column(db.Integer, primary_key=True)
    corrupted_observations = db.Column(db.Boolean)
    simulated = db.Column(db.Boolean)
    number_of_onlookers = db.Column(db.Integer)
    length_of_generations = db.Column(db.Integer)
    mutation_chance = db.Column(db.Float)
    cooperation_rate = db.Column(db.Integer)
    social_activeness = db.Column(db.Integer)
    positivity_of_gossip = db.Column(db.Integer)
    fitness = db.Column(db.Integer)
    generations = db.relationship('ReputationGeneration', backref='generation_reputation_community', lazy='dynamic')

    def set_corrupted(self):
        self.corrupted_observations = True
        db.session.commit()

    def set_not_corrupted(self, number_of_onlookers, length_of_generations, mutation_chance, cooperation_rate,
                          social_activeness, positivity_of_gossip, fitness):
        self.corrupted_observations = False
        self.number_of_onlookers = number_of_onlookers
        self.length_of_generations = length_of_generations
        self.mutation_chance = mutation_chance
        self.cooperation_rate = cooperation_rate
        self.social_activeness = social_activeness
        self.positivity_of_gossip = positivity_of_gossip
        self.fitness = fitness
        db.session.commit()

    def is_finished(self):
        return self.simulated


class ReputationGeneration(db.Model):
    """A generation of players which interact with each others"""
    community_id = db.Column(db.Integer, db.ForeignKey('reputation_community.id'))
    id = db.Column(db.Integer, primary_key=True)
    generation_id = db.Column(db.Integer)
    start_point = db.Column(db.Integer)
    end_point = db.Column(db.Integer)
    cooperation_rate = db.Column(db.Integer)
    social_activeness = db.Column(db.Integer)
    positivity_of_gossip = db.Column(db.Integer)
    fitness = db.Column(db.Integer)
    players = db.relationship('ReputationPlayer', backref='reputation_generation', lazy='dynamic',
                              primaryjoin="and_(ReputationGeneration.id==ReputationPlayer.generation_id,"
                                          "ReputationGeneration.community_id==ReputationPlayer.community_id)")

    def get_length(self):
        return self.end_point - self.start_point


class ReputationActionOnlookers(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    community_id = db.Column(db.Integer, db.ForeignKey('reputation_community.id'))
    generation_id = db.Column(db.Integer, db.ForeignKey('reputation_generation.id'))
    actor_id = db.Column(db.Integer, db.ForeignKey('reputation_player.id'))
    onlooker_id = db.Column(db.Integer, db.ForeignKey('reputation_player.id'))
    action_id = db.Column(db.Integer, db.ForeignKey('reputation_action.id'))


class ReputationPlayer(db.Model):
    """A player that has committed to actions based on their strategy and perception of the world"""
    generation_id = db.Column(db.Integer, db.ForeignKey('reputation_generation.id'))
    community_id = db.Column(db.Integer, db.ForeignKey('reputation_community.id'))
    id = db.Column(db.Integer, primary_key=True)
    player_id = db.Column(db.Integer)
    cooperation_rate = db.Column(db.Integer)
    social_activeness = db.Column(db.Integer)
    positivity_of_gossip = db.Column(db.Integer)
    fitness = db.Column(db.Integer)
    strategy = db.Column(db.Integer, db.ForeignKey('reputation_strategy.id'))
    actions = db.relationship("ReputationAction", backref='actor', lazy='dynamic',
                              primaryjoin="and_(ReputationPlayer.id==ReputationAction.player_id,"
                                          "ReputationPlayer.community_id==ReputationAction.community_id,"
                                          "ReputationPlayer.generation_id==ReputationAction.generation_id)")


class ReputationStrategy(db.Model):
    """A possible strategy of a reputation game player"""
    id = db.Column(db.Integer, primary_key=True)
    strategy_name = db.Column(db.String(128))
    strategy_options = db.Column(db.String(300))


from .indir_rec.action_logic import ActionType, GossipContent, InteractionContent


class ReputationAction(db.Model):
    """An action taken by a player in a reputation game"""
    generation_id = db.Column(db.Integer, db.ForeignKey('reputation_generation.id'))
    community_id = db.Column(db.Integer, db.ForeignKey('reputation_community.id'))
    player_id = db.Column(db.Integer, db.ForeignKey('reputation_player.id'))
    id = db.Column(db.Integer, primary_key=True)
    timepoint = db.Column(db.Integer, nullable=False)
    type = db.Column(db.Enum(ActionType), nullable=False)
    gossiper = db.Column(db.Integer, db.ForeignKey('reputation_player.id'), nullable=True)
    about = db.Column(db.Integer, db.ForeignKey('reputation_player.id'), nullable=True)
    recipient = db.Column(db.Integer, db.ForeignKey('reputation_player.id'), nullable=True)
    gossip = db.Column(db.Enum(GossipContent), nullable=True)
    donor = db.Column(db.Integer, db.ForeignKey('reputation_player.id'), nullable=True)
    action = db.Column(db.Enum(InteractionContent), nullable=True)

    def to_table_representation(self) -> List[str]:
        if self.type is ActionType.INTERACTION:
            return ["type: " + self.type.value['type'], "recipient: " + str(self.recipient), "action: " +
                    self.action.value['string']]
        elif self.type is ActionType.GOSSIP:
            return ["type: " + self.type.value['type'], "recipient: " + str(self.recipient), "about: " + str(self.about),
                    "gossip: " + self.gossip.value]
        else:
            return ["type: " + self.type.value['type']]

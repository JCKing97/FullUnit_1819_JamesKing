from app import db


class Strategy(db.Model):
    """The model for the strategy table in the database, used to store data on strategies in the system"""
    name = db.Column(db.String(64), primary_key=True, index=True)
    description = db.Column(db.String(256))

    def __repr__(self):
        return "<Strategy {}: {}>".format(self.name, self.description)


class Game(db.Model):
    """The model for the game table in the database, used to store data on games that have been finished"""
    id = db.Column(db.Integer, primary_key=True)
    actions = db.relationship('Action', backref='gameactions', lazy='dynamic')
    agents = db.relationship('Agent', secondary='game_agent_association', back_populates='games')
    tournament = db.Column(db.Integer, db.ForeignKey('tournament.id'))

    def get_interaction_history(self):
        interaction_history = Action.query.filter(Action.game_id == self.id)
        return interaction_history.order_by(Action.round_num.asc()).all()

    def __repr__(self):
        return "<Game: {}>".format(self.id)


class GameAgentAssociation(db.Model):
    agent_id = db.Column(db.Integer, db.ForeignKey('agent.id'), primary_key=True)
    game_id = db.Column(db.Integer, db.ForeignKey('game.id'), primary_key=True)


class Agent(db.Model):
    """The model for an agent that commits actions in specific rounds of specific games"""
    id = db.Column(db.Integer, primary_key=True)
    strategy_name = db.Column(db.String(64), db.ForeignKey('strategy.name'))
    games = db.relationship('Game', secondary='game_agent_association', back_populates='agents')
    community_id = db.Column(db.Integer, db.ForeignKey('community.id'))

    def __repr__(self):
        return "<Agent {}: {}>".format(self.id, self.strategy_name)


class Action(db.Model):
    """The model for an action in a game round by an agent"""
    game_id = db.Column(db.Integer, db.ForeignKey('game.id'), primary_key=True)
    round_num = db.Column(db.Integer, primary_key=True)
    agent_id = db.Column(db.Integer, db.ForeignKey('agent.id'), primary_key=True)
    cooperates = db.Column('cooperates', db.Boolean)

    def __repr__(self):
        return "<Action: game: {}, round: {}, agent: {}, cooperates: {}>".format(self.game_id, self.round_num, self.agent_id, self.cooperates)


class Community(db.Model):
    """The model for a community that is associated to agents"""
    id = db.Column(db.Integer, primary_key=True)
    agents = db.relationship('Agent', backref='community_agents', lazy='dynamic')
    tournaments = db.relationship('Tournament', backref='community_tournaments', lazy='dynamic')
    environment_id = db.Column(db.Integer, db.ForeignKey('environment.id'))

    def __repr__(self):
        return "<Community: {}, agents: {}>".format(self.id, self.agents)


class Tournament(db.Model):
    """The model for a tournament that is associated to its games"""
    id = db.Column(db.Integer, primary_key=True)
    games = db.relationship('Game', backref='tournament_agents', lazy='dynamic')
    community_id = db.Column(db.Integer, db.ForeignKey('community.id'))

    def __repr__(self):
        return "<Tournament: {}, games: {}>".format(self.id, self.games)


class Environment(db.Model):
    """The model for the environment that contains communities"""
    id = db.Column(db.Integer, primary_key=True)
    communities = db.relationship(Community, backref='community.id', lazy='dynamic')

    def __repr__(self):
        return "<Envirvonment: {}, communities: {}>".format(self.id, self.communities)


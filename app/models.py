from app import db


class Strategy(db.Model):
    name = db.Column(db.String(64), primary_key=True, index=True)
    description = db.Column(db.String(256))

    def __repr__(self):
        return "<Strategy {}: {}>".format(self.name, self.description)


class Game(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    rounds = db.relationship('Round', backref='roundsgame', lazy='dynamic')
    agents = db.relationship('Agent', backref='gameagent', lazy='dynamic')

    def get_interaction_history(self):
        interaction_history = Action.query.join(Round).filter(Round.game_id == self.id)
        return interaction_history.order_by(Action.round_id.desc())

    def __repr__(self):
        return "<Game: {}>".format(self.id)


class Agent(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    strategy_name = db.Column(db.String(64), db.ForeignKey('strategy.name'))
    game_id = db.Column(db.Integer, db.ForeignKey('game.id'))

    def __repr__(self):
        return "<Agent {}: {}>".format(self.id, self.strategy_name)


class Action(db.Model):
    round_id = db.Column(db.Integer, db.ForeignKey('round.id'), primary_key=True)
    agent_id = db.Column(db.Integer, db.ForeignKey('agent.id'), primary_key=True)
    cooperates = db.Column('cooperates', db.Boolean)

    def __repr__(self):
        return "<Action: round: {}, agent: {}, cooperates: {}>".format(self.round_id, self.agent_id, self.cooperates)


class Round(db.Model):
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    game_id = db.Column(db.Integer, db.ForeignKey('game.id'), primary_key=True)
    actions = db.relationship('Action', primaryjoin=(Action.round_id == id), backref='round_action', lazy='dynamic')

    def __repr__(self):
        return "<Round {}: game: {}>".format(self.id, self.game_id)
import unittest
from app import app, db
from app.models import Game, Agent, Action, GameAgentAssociation


class GameModelCase(unittest.TestCase):
    """Used to test the game model and in doing so other models related to it partially"""
    def setUp(self):
        app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite://'
        db.create_all()

    def tearDown(self):
        db.session.remove()
        db.drop_all()

    def test_interaction_history_normal(self):
        game = Game()
        agent1 = Agent(strategy_name='Tit-For-Tat')
        agent2 = Agent(strategy_name='Grudger')
        db.session.add_all([game, agent1, agent2])
        db.session.commit()
        agent1game1 = GameAgentAssociation(game_id=game.id, agent_id=agent1.id)
        agent2game1 = GameAgentAssociation(game_id=game.id, agent_id=agent2.id)
        actions = [Action(game_id=game.id, round_num=1, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game.id, round_num=1, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game.id, round_num=2, agent_id=agent1.id, cooperates=False),
                   Action(game_id=game.id, round_num=2, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game.id, round_num=3, agent_id=agent1.id, cooperates=False),
                   Action(game_id=game.id, round_num=3, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game.id, round_num=4, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game.id, round_num=4, agent_id=agent2.id, cooperates=False)]
        db.session.add_all([agent1game1, agent2game1])
        db.session.add_all(actions)
        db.session.commit()
        interaction_history = game.get_interaction_history()
        self.assertEqual(actions, interaction_history)

    def test_interaction_history_no_history(self):
        game = Game()
        agent1 = Agent(strategy_name='Tit-For-Tat')
        agent2 = Agent(strategy_name='Grudger')
        db.session.add_all([game, agent1, agent2])
        db.session.commit()
        agent1game1 = GameAgentAssociation(game_id=game.id, agent_id=agent1.id)
        agent2game1 = GameAgentAssociation(game_id=game.id, agent_id=agent2.id)
        db.session.add_all([agent1game1, agent2game1])
        db.session.commit()
        interaction_history = game.get_interaction_history()
        actions = []
        self.assertEqual(actions, interaction_history)

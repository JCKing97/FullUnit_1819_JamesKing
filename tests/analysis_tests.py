import unittest
from app import app, db
from app.analysis import get_game_points, get_tournament_points
from app.models import Game, Agent, Action, GameAgentAssociation, Tournament

class GameAnalysisCase(unittest.TestCase):
    """Used to test game analysis features"""
    def setUp(self):
        app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite://'
        db.create_all()

    def tearDown(self):
        db.session.remove()
        db.drop_all()

    def test_get_game_points_normal(self):
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
        player_points = get_game_points(game.get_interaction_history())
        agent1_points = 3 + 5 + 5 + 0
        agent2_points = 3 + 0 + 0 + 5
        self.assertEqual(agent1_points, player_points[agent1.id])
        self.assertEqual(agent2_points, player_points[agent2.id])

    def test_get_game_points_no_history(self):
        game = Game()
        agent1 = Agent(strategy_name='Tit-For-Tat')
        agent2 = Agent(strategy_name='Grudger')
        db.session.add_all([game, agent1, agent2])
        db.session.commit()
        agent1game1 = GameAgentAssociation(game_id=game.id, agent_id=agent1.id)
        agent2game1 = GameAgentAssociation(game_id=game.id, agent_id=agent2.id)
        db.session.add_all([agent1game1, agent2game1])
        db.session.commit()
        player_points = get_game_points(game.get_interaction_history())
        self.assertEqual({}, player_points)


class TournamentAnalysisCase(unittest.TestCase):
    """Used to test tournament analysis features"""

    def setUp(self):
        app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite://'
        db.create_all()

    def tearDown(self):
        db.session.remove()
        db.drop_all()

    def test_get_tournaments_points_normal(self):
        tournament = Tournament()
        db.session.add(tournament)
        db.session.commit()
        game1 = Game(tournament=tournament.id)
        game2 = Game(tournament=tournament.id)
        game3 = Game(tournament=tournament.id)
        game4 = Game(tournament=tournament.id)
        game5 = Game(tournament=tournament.id)
        game6 = Game(tournament=tournament.id)
        agent1 = Agent(strategy_name='Tit-For-Tat')
        agent2 = Agent(strategy_name='Grudger')
        agent3 = Agent(strategy_name='Always Cooperate')
        agent4 = Agent(strategy_name='Always Defect')
        db.session.add_all([game1, game2, game3, game4, game5, game6, agent1, agent2, agent3, agent4])
        db.session.commit()
        agent_game_associations = [GameAgentAssociation(game_id=game1.id, agent_id=agent1.id),
                                   GameAgentAssociation(game_id=game1.id, agent_id=agent2.id),
                                   GameAgentAssociation(game_id=game2.id, agent_id=agent1.id),
                                   GameAgentAssociation(game_id=game2.id, agent_id=agent3.id),
                                   GameAgentAssociation(game_id=game3.id, agent_id=agent1.id),
                                   GameAgentAssociation(game_id=game3.id, agent_id=agent4.id),
                                   GameAgentAssociation(game_id=game4.id, agent_id=agent2.id),
                                   GameAgentAssociation(game_id=game4.id, agent_id=agent3.id),
                                   GameAgentAssociation(game_id=game5.id, agent_id=agent2.id),
                                   GameAgentAssociation(game_id=game5.id, agent_id=agent4.id),
                                   GameAgentAssociation(game_id=game6.id, agent_id=agent3.id),
                                   GameAgentAssociation(game_id=game6.id, agent_id=agent4.id)]
        actions = [Action(game_id=game1.id, round_num=1, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=1, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game1.id, round_num=2, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=2, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game1.id, round_num=3, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=3, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game1.id, round_num=4, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=4, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game2.id, round_num=1, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game2.id, round_num=1, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game2.id, round_num=2, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game2.id, round_num=2, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game2.id, round_num=3, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game2.id, round_num=3, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game2.id, round_num=4, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game2.id, round_num=4, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game3.id, round_num=1, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game3.id, round_num=1, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game3.id, round_num=2, agent_id=agent1.id, cooperates=False),
                   Action(game_id=game3.id, round_num=2, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game3.id, round_num=3, agent_id=agent1.id, cooperates=False),
                   Action(game_id=game3.id, round_num=3, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game3.id, round_num=4, agent_id=agent1.id, cooperates=False),
                   Action(game_id=game3.id, round_num=4, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game4.id, round_num=1, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game4.id, round_num=1, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game4.id, round_num=2, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game4.id, round_num=2, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game4.id, round_num=3, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game4.id, round_num=3, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game4.id, round_num=4, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game4.id, round_num=4, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game5.id, round_num=1, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game5.id, round_num=1, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game5.id, round_num=2, agent_id=agent2.id, cooperates=False),
                   Action(game_id=game5.id, round_num=2, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game5.id, round_num=3, agent_id=agent2.id, cooperates=False),
                   Action(game_id=game5.id, round_num=3, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game5.id, round_num=4, agent_id=agent2.id, cooperates=False),
                   Action(game_id=game5.id, round_num=4, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game6.id, round_num=1, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game6.id, round_num=1, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game6.id, round_num=2, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game6.id, round_num=2, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game6.id, round_num=3, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game6.id, round_num=3, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game6.id, round_num=4, agent_id=agent3.id, cooperates=True),
                   Action(game_id=game6.id, round_num=4, agent_id=agent4.id, cooperates=False)]
        db.session.add_all(agent_game_associations)
        db.session.add_all(actions)
        db.session.commit()
        player_points = get_tournament_points(tournament.games)
        agent1_points = (3 + 3 + 3 + 3) + (3 + 3 + 3 + 3) + (0 + 1 + 1 + 1)
        agent2_points = (3 + 3 + 3 + 3) + (3 + 3 + 3 + 3) + (0 + 1 + 1 + 1)
        agent3_points = (3 + 3 + 3 + 3) + (3 + 3 + 3 + 3) + (0 + 0 + 0 + 0)
        agent4_points = (5 + 1 + 1 + 1) + (5 + 1 + 1 + 1) + (5 + 5 + 5 + 5)
        self.assertEqual(agent1_points, player_points[agent1.id])
        self.assertEqual(agent2_points, player_points[agent2.id])
        self.assertEqual(agent3_points, player_points[agent3.id])
        self.assertEqual(agent4_points, player_points[agent4.id])

    def test_get_tournament_points_no_games(self):
        tournament = Tournament()
        db.session.add(tournament)
        db.session.commit()
        player_points = get_tournament_points(tournament.games)
        self.assertEqual({}, player_points)

    def test_get_tournament_points_empty_games(self):
        tournament = Tournament()
        db.session.add(tournament)
        db.session.commit()
        game1 = Game(tournament=tournament.id)
        game2 = Game(tournament=tournament.id)
        game3 = Game(tournament=tournament.id)
        agent1 = Agent(strategy_name='Tit-For-Tat')
        agent2 = Agent(strategy_name='Grudger')
        agent3 = Agent(strategy_name='Always Cooperate')
        agent4 = Agent(strategy_name='Always Defect')
        db.session.add_all([game1, game2, game3, agent1, agent2, agent3, agent4])
        db.session.commit()
        agent_game_associations = [GameAgentAssociation(game_id=game1.id, agent_id=agent1.id),
                                   GameAgentAssociation(game_id=game1.id, agent_id=agent2.id),
                                   GameAgentAssociation(game_id=game2.id, agent_id=agent3.id),
                                   GameAgentAssociation(game_id=game2.id, agent_id=agent4.id),
                                   GameAgentAssociation(game_id=game3.id, agent_id=agent2.id),
                                   GameAgentAssociation(game_id=game3.id, agent_id=agent4.id)]
        actions = [Action(game_id=game1.id, round_num=1, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=1, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game1.id, round_num=2, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=2, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game1.id, round_num=3, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=3, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game1.id, round_num=4, agent_id=agent1.id, cooperates=True),
                   Action(game_id=game1.id, round_num=4, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game3.id, round_num=1, agent_id=agent2.id, cooperates=True),
                   Action(game_id=game3.id, round_num=1, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game3.id, round_num=2, agent_id=agent2.id, cooperates=False),
                   Action(game_id=game3.id, round_num=2, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game3.id, round_num=3, agent_id=agent2.id, cooperates=False),
                   Action(game_id=game3.id, round_num=3, agent_id=agent4.id, cooperates=False),
                   Action(game_id=game3.id, round_num=4, agent_id=agent2.id, cooperates=False),
                   Action(game_id=game3.id, round_num=4, agent_id=agent4.id, cooperates=False)]
        db.session.add_all(agent_game_associations)
        db.session.add_all(actions)
        db.session.commit()
        player_points = get_tournament_points(tournament.games)
        agent1_points = 3 + 3 + 3 + 3
        agent2_points = 3 + 3 + 3 + 3 + 0 + 1 + 1 + 1
        agent4_points = 5 + 1 + 1 + 1
        self.assertEqual(agent1_points, player_points[agent1.id])
        self.assertEqual(agent2_points, player_points[agent2.id])
        self.assertEqual(agent4_points, player_points[agent4.id])
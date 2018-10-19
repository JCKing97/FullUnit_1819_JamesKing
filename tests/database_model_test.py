"""database_model_test.py: Test the model"""

__author__ = "James King adapted from Miguel Grinberg"

from app import db, create_app
from app.models import Match, Round, Player, Action
from tests.test_config import TestConfig
import unittest

class MatchTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        db.create_all()

    def tearDown(self):
        db.session.remove()
        db.drop_all()
        self.app_context.pop()

    def test_interaction_history_normal(self):
        match = Match()
        player1 = Player(id=1, match_id=1, strategy='Tit-For-Tat')
        player2 = Player(id=2, match_id=1, strategy='Grudger')
        round1 = Round(num=1, match_id=1)
        round2 = Round(num=2, match_id=1)
        round3 = Round(num=3, match_id=1)
        round4 = Round(num=4, match_id=1)
        db.session.add_all([match, player1, player2, round1, round2, round3, round4])
        db.session.commit()
        actions = [Action(match_id=match.id, round_num=1, player_id=player1.id, cooperate=True),
                   Action(match_id=match.id, round_num=1, player_id=player2.id, cooperate=True),
                   Action(match_id=match.id, round_num=2, player_id=player1.id, cooperate=False),
                   Action(match_id=match.id, round_num=2, player_id=player2.id, cooperate=True),
                   Action(match_id=match.id, round_num=3, player_id=player1.id, cooperate=False),
                   Action(match_id=match.id, round_num=3, player_id=player2.id, cooperate=True),
                   Action(match_id=match.id, round_num=4, player_id=player1.id, cooperate=True),
                   Action(match_id=match.id, round_num=4, player_id=player2.id, cooperate=False)]
        db.session.add_all(actions)
        db.session.commit()
        interaction_history = match.get_interaction_history()
        self.assertEqual(actions, interaction_history)

    def test_interaction_history_no_history(self):
        match = Match()
        player1 = Player(id=1, match_id=1, strategy='Tit-For-Tat')
        player2 = Player(id=2, match_id=1, strategy='Grudger')
        db.session.add_all([match, player1, player2])
        db.session.commit()
        interaction_history = match.get_interaction_history()
        actions = []
        self.assertEqual(actions, interaction_history)

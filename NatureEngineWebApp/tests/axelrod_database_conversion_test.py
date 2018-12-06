"""axelrod_database_conversion_test.py: A set of test caases to test the conversion between my database and the axelrod library representation"""

__author__ = "James King adapted from Miguel Grinberg"

import unittest
import axelrod
from app.main.axelrod_database_conversion import match_result_to_database
from app import create_app, db
from app.models import Match, Player, Round, Action
from tests.test_config import TestConfig

class TestConversion(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        db.create_all()

    def tearDown(self):
        db.session.remove()
        db.drop_all()
        self.app_context.pop()

    def test_match_result_to_database_tft_defector(self):
        players = (axelrod.TitForTat(), axelrod.Defector())
        check_players = [player.name for player in players]
        results = axelrod.Match(players=players, turns=5).play()
        match_id = match_result_to_database(results, players)
        self.assertTrue(match_id==1)
        self.assertTrue(len(Match.query.all())==1)
        self.assertTrue(len(Round.query.filter_by(match_id=1).all())==5)
        db_players = Player.query.all()
        for player in db_players:
            self.assertTrue(player.strategy in check_players)
            check_players.remove(player.strategy)
        db_actions = Action.query.all()
        for action in db_actions:
            if action.player.strategy == axelrod.TitForTat().name and action.round_num == 1:
                self.assertTrue(action.cooperate)
            else:
                self.assertFalse(action.cooperate)

    def test_match_result_to_database_cooperators(self):
        players = (axelrod.Cooperator(), axelrod.Cooperator())
        check_players = [player.name for player in players]
        results = axelrod.Match(players=players, turns=19).play()
        match_id = match_result_to_database(results, players)
        self.assertTrue(match_id==1)
        self.assertTrue(len(Match.query.all())==1)
        self.assertTrue(len(Round.query.filter_by(match_id=1).all())==19)
        db_players = Player.query.all()
        for player in db_players:
            self.assertTrue(player.strategy in check_players)
            check_players.remove(player.strategy)
        db_actions = Action.query.all()
        for action in db_actions:
            self.assertTrue(action.cooperate)

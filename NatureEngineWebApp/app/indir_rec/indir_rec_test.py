"""indir_rec_test.py: Test the indirect reciprocity model implementation"""

__author__ = "James King adapted from Miguel Grinberg"

import unittest
from tests.test_config import TestConfig
from app import create_app
from app.indir_rec.player_logic import PlayerFactory, PlayerCreationException


class PlayerTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.factory = PlayerFactory()

    def tearDown(self):
        self.app_context.pop()

    def test_create_defector(self):
        try:
            self.factory.new_player("defector")
        except PlayerCreationException:
            self.fail("Should not have raised an exception, player information was correct")

    def test_create_2_defector_2_coop(self):
        players = []
        try:
            for i in range(4):
                if i < 2:
                    players.append(self.factory.new_player("defector"))
                else:
                    players.append(self.factory.new_player("cooperator"))
        except PlayerCreationException:
            self.fail("Should not have raised exception, player information was correct")

    def test_create_incorrect(self):
        with self.assertRaises(PlayerCreationException):
            self.factory.new_player("no strategy")

    def test_create_correct_then_incorrect(self):
        try:
            self.factory.new_player("defector")
        except PlayerCreationException:
            self.fail("Should not have raised an exception, player information was correct")
        with self.assertRaises(PlayerCreationException):
            self.factory.new_player("no strategy")

    def test_create_incorrect_then_correct(self):
        with self.assertRaises(PlayerCreationException):
            self.factory.new_player("no strategy")
        try:
            self.factory.new_player("defector")
        except PlayerCreationException:
            self.fail("Should not have raised an exception, player information was correct")

    def test_defector_get_action(self):
        player = self.factory.new_player("defector")
        other_player = self.factory.new_player("defector")
        self.assertTrue(player.get_action(other_player) == "defect")

    def test_cooperator_get_action(self):
        player = self.factory.new_player("cooperator")
        other_player = self.factory.new_player("defector")
        self.assertTrue(player.get_action(other_player) == "cooperate")

    def test_fitness(self):
        player = self.factory.new_player("cooperator")
        self.assertTrue(player.get_fitness() == 0)
        player.update_fitness(2)
        self.assertTrue(player.get_fitness() == 2)
        player.update_fitness(2)
        self.assertTrue(player.get_fitness() == 4)
        player.update_fitness(-1)
        self.assertTrue(player.get_fitness() == 3)
        player.update_fitness(0)
        self.assertTrue(player.get_fitness() == 3)






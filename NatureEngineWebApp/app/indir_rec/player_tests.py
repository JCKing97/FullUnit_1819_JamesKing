"""player_tests.py: Test the Player class"""

from .player_logic import Player
from app import create_app
from tests.test_config import TestConfig
import unittest
import requests
from flask import current_app


class PlayerTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']

    def tearDown(self):
        self.app_context.pop()

    def test_player_get_id(self):
        player = Player(0)
        self.assertEqual(0, player.get_id())

    def test_player_get_fitness(self):
        player = Player(0)
        self.assertEqual(0, player.get_fitness())

    def test_player_update_get_fitness(self):
        player = Player(0)
        player.update_fitness(-4)
        self.assertEqual(0, player.get_fitness())
        player.update_fitness(7)
        self.assertEqual(7, player.get_fitness())

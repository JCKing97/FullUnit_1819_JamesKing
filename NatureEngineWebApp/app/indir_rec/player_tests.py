"""player_tests.py: Test the functionality of the player_logic.py module"""

from .player_logic import Player, DecisionException, PlayerCreationException
from app import create_app
from tests.test_config import TestConfig
import unittest
import requests
from flask import current_app


class PlayerTest(unittest.TestCase):
    """Test the Player class"""

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", current_app.config['AGENTS_URL'] + 'community').json()['id']
        self.generation = 0
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", current_app.config['AGENTS_URL'] + 'generation', json=generation_payload)

    def tearDown(self):
        self.app_context.pop()

    def test_player_get_id(self):
        """Test the get_id() method of the Player class"""
        try:
            player = Player(0, {'name': "Defector", 'options': []}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        self.assertEqual(0, player.get_id())

    def test_player_get_fitness_start(self):
        """Test the get_fitness method of the player at the start of it's life"""
        try:
            player = Player(0, {'name': "Defector", 'options': []}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        self.assertEqual(0, player.get_fitness())

    def test_player_update_get_fitness(self):
        """Test the update_fitness() and get_fitness methods of a player together"""
        try:
            player = Player(0, {'name': "Defector", 'options': []}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        player.update_fitness(-4)
        self.assertEqual(0, player.get_fitness())
        player.update_fitness(7)
        self.assertEqual(7, player.get_fitness())

    def test_player_decide(self):
        """Test the decision making of the player"""
        try:
            player = Player(0, {'name': "Defector", 'options': []}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        try:
            self.assertEqual({'type': 'idle'}, player.decide(2))
        except DecisionException:
            self.fail("Shouldn't have failed to make a decision")



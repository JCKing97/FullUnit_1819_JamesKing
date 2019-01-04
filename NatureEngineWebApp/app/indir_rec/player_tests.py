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

    def test_create_player(self):
        """Test creating a player"""
        try:
            Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")

    def test_incorrect_strategy_create_player(self):
        """Test creating a player with a strategy not in the system"""
        with self.assertRaises(PlayerCreationException):
            Player(0, {'name': "Incorrect", 'options': []}, self.community, self.generation)

    def test_multiple_players_same_id(self):
        Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        with self.assertRaises(PlayerCreationException):
            Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)

    def test_player_get_id(self):
        """Test the get_id() method of the Player class"""
        try:
            player = Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        self.assertEqual(0, player.get_id())

    def test_player_get_fitness_start(self):
        """Test the get_fitness method of the player at the start of it's life"""
        try:
            player = Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        self.assertEqual(0, player.get_fitness())

    def test_player_update_get_fitness(self):
        """Test the update_fitness() and get_fitness methods of a player together"""
        try:
            player = Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        player.update_fitness(-4)
        self.assertEqual(0, player.get_fitness())
        player.update_fitness(7)
        self.assertEqual(7, player.get_fitness())

    def test_player_decide(self):
        """Test the decision making of the player"""
        try:
            player = Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        try:
            self.assertEqual({'type': 'idle'}, player.decide(2))
        except DecisionException:
            self.fail("Shouldn't have failed to make a decision")

    def test_send_donor_percept(self):
        """Test sending a percept to a player that they are a donor, and getting a decision to defect back"""
        try:
            donor = Player(0, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
            recipient = Player(1, {'name': "Defector", 'options': ["lazy"]}, self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        interaction_payload = {'donor': donor.get_id(), 'recipient': recipient.get_id(), 'timepoint': 2,
         'community': self.community, 'generation': self.generation}
        interaction_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/interaction',
                                                json=interaction_payload)
        self.assertEqual(interaction_response.status_code, 200)
        self.assertTrue(interaction_response.json()['success'], msg="Should have been successful sending this percept")
        try:
            self.assertEqual({'type': 'action', 'value': 'defect', 'recipient': recipient.get_id()}, donor.decide(2),
                             msg="Should have defected")
        except DecisionException:
            self.fail("Shouldn't have failed to make a decision")




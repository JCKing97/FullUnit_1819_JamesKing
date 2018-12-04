"""generation_tests.py: Tests for the functionality of the generation_logic.py module"""

from app import create_app
from tests.test_config import TestConfig
import unittest
import requests
from flask import current_app
from .generation_logic import Generation, GenerationCreationException, SimulationException
import random


class GenerationTest(unittest.TestCase):
    """Test the Generation class"""

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", current_app.config['AGENTS_URL'] + 'community').json()['id']

    def tearDown(self):
        self.app_context.pop()

    def test_get_id(self):
        try:
            strategies = [
                {'strategy': {'name': 'Defector', 'options': [], 'description': 'Always defect'}, 'count': 3},
                {'strategy': {'name': 'Cooperator', 'options': [], 'description': 'Always cooperate'}, 'count': 1},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['trusting'],
                              'description': 'Trust gossip from trusted agents, use the standing strat'}, 'count': 0},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['distrusting'],
                              'description': 'Don\'t trust, use the standing strat'}, 'count': 9}
            ]
            generation = Generation(strategies, 0, self.community, 0, 10, 8)
            self.assertEqual(0, generation.get_id())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_create_start_point_less_than_end_point(self):
        with self.assertRaises(GenerationCreationException):
            strategies = [
                {'strategy': {'name': 'Defector', 'options': [], 'description': 'Always defect'}, 'count': 3},
                {'strategy': {'name': 'Cooperator', 'options': [], 'description': 'Always cooperate'}, 'count': 1},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['trusting'],
                              'description': 'Trust gossip from trusted agents, use the standing strat'}, 'count': 0},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['distrusting'],
                              'description': 'Don\'t trust, use the standing strat'}, 'count': -7}
            ]
            Generation(strategies, 0, self.community, 20, 10, 8)

    def test_create_start_point_equal_to_end_point(self):
        with self.assertRaises(GenerationCreationException):
            strategies = [
                {'strategy': {'name': 'Defector', 'options': [], 'description': 'Always defect'}, 'count': 3},
                {'strategy': {'name': 'Cooperator', 'options': [], 'description': 'Always cooperate'}, 'count': 1},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['trusting'],
                              'description': 'Trust gossip from trusted agents, use the standing strat'}, 'count': 0},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['distrusting'],
                              'description': 'Don\'t trust, use the standing strat'}, 'count': 9}
            ]
            Generation(strategies, 0, self.community, 10, 10, 8)

    def test_get_start_point(self):
        try:
            strategies = [
                {'strategy': {'name': 'Defector', 'options': [], 'description': 'Always defect'}, 'count': 3},
                {'strategy': {'name': 'Cooperator', 'options': [], 'description': 'Always cooperate'}, 'count': 1},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['trusting'],
                              'description': 'Trust gossip from trusted agents, use the standing strat'}, 'count': 0},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['distrusting'],
                              'description': 'Don\'t trust, use the standing strat'}, 'count': 9}
            ]
            generation = Generation(strategies, 0, self.community, 0, 10, 8)
            self.assertEqual(0, generation.get_start_point())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_get_end_point(self):
        try:
            strategies = [
                {'strategy': {'name': 'Defector', 'options': [], 'description': 'Always defect'}, 'count': 3},
                {'strategy': {'name': 'Cooperator', 'options': [], 'description': 'Always cooperate'}, 'count': 1},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['trusting'],
                              'description': 'Trust gossip from trusted agents, use the standing strat'}, 'count': 0},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['distrusting'],
                              'description': 'Don\'t trust, use the standing strat'}, 'count': 9}
            ]
            generation = Generation(strategies, 0, self.community, 0, 10, 8)
            self.assertEqual(10, generation.get_end_point())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_simulate(self):
        try:
            strategies = [
                {'strategy': {'name': 'Defector', 'options': [], 'description': 'Always defect'},
                 'count': random.randint(0, 20)},
                {'strategy': {'name': 'Cooperator', 'options': [], 'description': 'Always cooperate'},
                 'count': random.randint(0, 20)},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['trusting'],
                              'description': 'Trust gossip from trusted agents, use the standing strat'},
                 'count': random.randint(0, 20)},
                {'strategy': {'name': 'Standing Discriminator', 'options': ['distrusting'],
                              'description': 'Don\'t trust, use the standing strat'}, 'count': random.randint(0, 20)}
            ]
            generation = Generation(strategies=strategies, generation_id=0, community_id=self.community, start_point=0,
                                    end_point=10, num_of_onlookers=8)
            generation.simulate()
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")
        except SimulationException:
            self.fail("Should not fail to simulate")



"""community_tests.py: tests the functionality in the community_logic.py module"""

from app import create_app
from tests.test_config import TestConfig
import unittest
from .community_logic import Community
import requests
from flask import current_app
import random


class CommunityTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()

    def tearDown(self):
        self.app_context.pop()

    def test_get_id(self):
        new_community_id = requests.request("POST", current_app.config['AGENTS_URL'] + 'community').json()['id']
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
        num_of_onlookers = random.randint(6, 100)
        num_of_generations = random.randint(3, 20)
        length_of_generations = random.randint(6, 50)
        community = Community(strategies, num_of_onlookers, num_of_generations, length_of_generations)
        self.assertEqual(new_community_id+1, community.get_id())

    def test_get_onlookers(self):
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
        num_of_onlookers = random.randint(6, 100)
        num_of_generations = random.randint(3, 20)
        length_of_generations = random.randint(6, 50)
        community = Community(strategies, num_of_onlookers, num_of_generations, length_of_generations)
        self.assertEqual(num_of_onlookers, community.get_num_of_onlookers())

    def test_simulate(self):
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
        num_of_onlookers = random.randint(6, 100)
        num_of_generations = random.randint(3, 20)
        length_of_generations = random.randint(6, 50)
        community = Community(strategies, num_of_onlookers, num_of_generations, length_of_generations)
        community.simulate()





"""community_tests.py: tests the functionality in the community_logic.py module"""

from app import create_app
from tests.test_config import TestConfig
import unittest
from .community_logic import Community
import requests
import random
import pprint
from .indir_rec_config import Config
from .strategy_logic import Strategy


class CommunityTest(unittest.TestCase):
    """Test the community class"""

    @classmethod
    def setUpClass(cls):
        # Initial set up the runs once
        # Set up the initial set of strategies for each community
        response = requests.get('http://127.0.0.1:8080/strategy')
        received_strategies = response.json()['strategies']
        cls.strat_count = 0
        cls.strategies = {}
        for strategy in received_strategies:
            count = random.randint(0, 1)
            cls.strategies[Strategy(strategy['donor_strategy'], strategy['non_donor_strategy'],
                                    strategy['trust_model'], strategy['options'])] = count
            cls.strat_count += count
        cls.pp = pprint.PrettyPrinter()
        # Set up the relevant parameters for each generation
        cls.num_of_onlookers = random.randint(1, 10)
        cls.num_of_generations = random.randint(3, 5)
        cls.length_of_generations = random.randint(6, 10)

    def setUp(self):
        # Build the app to test on
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()

    def tearDown(self):
        # Remove the app
        self.app_context.pop()

    def test_get_id(self):
        # Create a community and test that the id of the community is correctly returned
        # (I don't know why I didn't use property getters)
        new_community_id = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        community = Community(self.strategies, self.num_of_onlookers, self.num_of_generations,
                              self.length_of_generations)
        self.assertEqual(new_community_id+1, community.get_id())

    def test_get_onlookers(self):
        # Test that the set number of onlookers is returned correctly by the getter
        # (I don't know why I didn't use property getters)
        community = Community(self.strategies, self.num_of_onlookers, self.num_of_generations,
                              self.length_of_generations)
        self.assertEqual(self.num_of_onlookers, community.get_num_of_onlookers())

    def test_simulate(self):
        # Test a simulation of a community
        community = Community(self.strategies, self.num_of_onlookers, self.num_of_generations,
                              self.length_of_generations)
        community.simulate()
        # Test that the correct amount of generations were simulated
        self.assertEqual(self.num_of_generations, len(community.get_generations()))
        for generation in community.get_generations():
            # Each generation should run for the specified time and have the same amount of players
            self.assertEqual(self.length_of_generations, generation.get_end_point()-generation.get_start_point())
            self.assertEqual(self.strat_count, len(generation.get_players()))








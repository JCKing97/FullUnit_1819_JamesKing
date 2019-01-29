"""community_tests.py: tests the functionality in the community_logic.py module"""

from app import create_app
from tests.test_config import TestConfig
import unittest
from .community_logic import Community
import requests
from flask import current_app
import random
import pprint
from .indir_rec_config import Config
from .strategy_logic import Strategy


class CommunityTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        response = requests.get('http://127.0.0.1:8080/strategy')
        received_strategies = response.json()['strategies']
        cls.strat_count = 0
        cls.strategies = {}
        for strategy in received_strategies:
            count = random.randint(0, 5)
            cls.strategies[Strategy(strategy['name'], strategy['options'])] = count
            cls.strat_count += count
        cls.pp = pprint.PrettyPrinter()
        cls.num_of_onlookers = random.randint(1, 20)
        cls.num_of_generations = random.randint(3, 8)
        cls.length_of_generations = random.randint(6, 20)

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()

    def tearDown(self):
        self.app_context.pop()

    def test_get_id(self):
        new_community_id = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        community = Community(self.strategies, self.num_of_onlookers, self.num_of_generations,
                              self.length_of_generations)
        self.assertEqual(new_community_id+1, community.get_id())

    def test_get_onlookers(self):
        community = Community(self.strategies, self.num_of_onlookers, self.num_of_generations,
                              self.length_of_generations)
        self.assertEqual(self.num_of_onlookers, community.get_num_of_onlookers())

    def test_simulate(self):
        community = Community(self.strategies, self.num_of_onlookers, self.num_of_generations,
                              self.length_of_generations)
        community.simulate()
        self.assertEqual(self.num_of_generations, len(community.get_generations()))
        for generation in community.get_generations():
            self.assertEqual(self.length_of_generations, generation.get_end_point()-generation.get_start_point())
            self.assertEqual(self.strat_count, len(generation.get_players()))








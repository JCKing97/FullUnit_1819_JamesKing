"""generation_tests.py: Tests for the functionality of the generation_logic.py module"""

from app import create_app
from tests.test_config import TestConfig
import unittest
import requests
from flask import current_app
from .generation_logic import Generation, GenerationCreationException, SimulationException
from .action_logic import InteractionAction, InteractionContent
import random
import pprint


class GenerationTest(unittest.TestCase):
    """Test the Generation class"""

    @classmethod
    def setUpClass(cls):
        response = requests.get('http://127.0.0.1:8080/strategy')
        received_strategies = response.json()['strategies']
        cls.initial_strategies = [{'strategy': strategy, 'count': random.randint(0, 5)} for strategy in received_strategies]
        cls.non_initial_strategies = []
        for strategy in received_strategies:
            for i in range(random.randint(0,5)):
                cls.non_initial_strategies.append(strategy)
        cls.num_of_onlookers = random.randint(1, 20)
        cls.start_point = random.randint(0, 10)
        cls.end_point = random.randint(15, 25)
        cls.initial_gen = True

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", current_app.config['AGENTS_URL'] + 'community').json()['id']

    def tearDown(self):
        self.app_context.pop()

    def test_get_id(self):
        try:
            generation = Generation(self.initial_strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, self.initial_gen, [])
            self.assertEqual(0, generation.id)
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_create_start_point_less_than_end_point(self):
        with self.assertRaises(GenerationCreationException):
            Generation(self.initial_strategies, 0, self.community, 10, 8, self.num_of_onlookers, self.initial_gen, [])

    def test_create_start_point_equal_to_end_point(self):
        with self.assertRaises(GenerationCreationException):
            Generation(self.initial_strategies, 0, self.community, 10, 10, self.num_of_onlookers, self.initial_gen, [])

    def test_create_non_initial(self):
        try:
            Generation(self.non_initial_strategies, 0, self.community, self.start_point, self.end_point,
                       self.num_of_onlookers, False, [])
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_get_start_point(self):
        try:
            generation = Generation(self.initial_strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, self.initial_gen, [])
            self.assertEqual(self.start_point, generation.get_start_point())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_get_end_point(self):
        try:
            generation = Generation(self.non_initial_strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, False, [])
            self.assertEqual(self.end_point, generation.get_end_point())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_get_onlookers(self):
        try:
            generation = Generation(self.non_initial_strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, False, [])
            donor_id = random.randint(0, len(generation.get_players())-1)
            recipient_id = random.randint(0, len(generation.get_players())-1)
            action = InteractionAction(0, donor_id, generation.id, recipient_id,
                                       InteractionContent.COOPERATE)
            onlookers = generation._generate_onlookers(action)
            self.assertEqual(self.num_of_onlookers+2, len(onlookers))
            self.assertTrue(generation._id_player_map[donor_id] in onlookers, self.initial_gen)
            self.assertTrue(generation._id_player_map[recipient_id] in onlookers, self.initial_gen)
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    # def test_simulate(self):
    #     try:
    #         generation = Generation(self.initial_strategies, 0, self.community, self.start_point, self.end_point,
    #                                 self.num_of_onlookers, self.initial_gen)
    #         generation.simulate()
    #         num_of_players = 0
    #         for strategy in self.initial_strategies:
    #             num_of_players += strategy['count']
    #         actions = generation._actions
    #         timepoints = 0
    #         for timepoint in actions:
    #             timepoints += 1
    #             self.assertEqual(num_of_players, len(actions[timepoint]))
    #         self.assertEqual(timepoints, self.end_point - self.start_point)
    #         percepts = {}
    #         for player in generation.get_players():
    #             for timepoint in player._percepts:
    #                 if timepoint in percepts:
    #                     for percept in player._percepts[timepoint]:
    #                         percepts[timepoint].append(percept)
    #                 else:
    #                     percepts[timepoint] = player._percepts[timepoint]
    #         self.assertEqual(self.end_point-self.start_point, len(percepts))
    #         for timepoint in percepts:
    #             # Calculate number of percepts there should be
    #             num_of_expected_percepts = 0
    #             if timepoint in actions:
    #                 num_of_expected_percepts += 2 + self.num_of_onlookers
    #                 for action in actions[timepoint]:
    #                     if action['type'] == 'gossip':
    #                         num_of_expected_percepts += 1
    #             self.assertEqual(num_of_expected_percepts, len(percepts[timepoint]))
    #         print("Actions: ")
    #         self.pp.pprint(actions)
    #         print("Percepts: ")
    #         self.pp.pprint(percepts)
    #     except GenerationCreationException:
    #         self.fail("Should not have failed to create generation")
    #     except SimulationException:
    #         self.fail("Should not fail to simulate")
    #
    # def test_simulate_non_initial(self):
    #     try:
    #         generation = Generation(self.non_initial_strategies, 0, self.community, self.start_point, self.end_point,
    #                                 self.num_of_onlookers, False)
    #         generation.simulate()
    #         num_of_players = len(self.non_initial_strategies)
    #         actions = generation._actions
    #         timepoints = 0
    #         for timepoint in actions:
    #             timepoints += 1
    #             self.assertEqual(num_of_players, len(actions[timepoint]))
    #         self.assertEqual(timepoints, self.end_point - self.start_point)
    #         percepts = {}
    #         for player in generation.get_players():
    #             for timepoint in player._percepts:
    #                 if timepoint in percepts:
    #                     for percept in player._percepts[timepoint]:
    #                         percepts[timepoint].append(percept)
    #                 else:
    #                     percepts[timepoint] = player._percepts[timepoint]
    #         self.assertEqual(self.end_point-self.start_point, len(percepts))
    #         for timepoint in percepts:
    #             # Calculate number of percepts there should be
    #             num_of_expected_percepts = 0
    #             if timepoint in actions:
    #                 num_of_expected_percepts += 2 + self.num_of_onlookers
    #                 for action in actions[timepoint]:
    #                     if action['type'] == 'gossip':
    #                         num_of_expected_percepts += 1
    #             self.assertEqual(num_of_expected_percepts, len(percepts[timepoint]))
    #         print("Actions: ")
    #         self.pp.pprint(actions)
    #         print("Percepts: ")
    #         self.pp.pprint(percepts)
    #     except GenerationCreationException:
    #         self.fail("Should not have failed to create generation")
    #     except SimulationException:
    #         self.fail("Should not fail to simulate")
    #
    #

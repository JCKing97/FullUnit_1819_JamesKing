"""generation_tests.py: Tests for the functionality of the generation_logic.py module"""

from app import create_app
from tests.test_config import TestConfig
import unittest
import requests
from .generation_logic import Generation, GenerationCreationException, SimulationException
from .action_logic import InteractionAction, InteractionContent, ActionType
from .observation_logic import ActionObserver
import random
import pprint
from .indir_rec_config import Config
from .strategy_logic import Strategy


class GenerationTest(unittest.TestCase):
    """Test the Generation class"""

    @classmethod
    def setUpClass(cls):
        # Create a set of strategies for each generation
        response = requests.get('http://127.0.0.1:8080/strategy')
        received_strategies = response.json()['strategies']
        cls.strategies = {}
        cls.strat_count = 0
        for strategy in received_strategies:
            count = random.randint(0, 1)
            cls.strategies[Strategy(strategy['donor_strategy'], strategy['non_donor_strategy'],
                                    strategy['trust_model'], strategy['options'])] = count
            cls.strat_count += count
        # Set up relevant parameters
        cls.num_of_onlookers = random.randint(1, 10)
        cls.start_point = random.randint(5, 10)
        cls.end_point = random.randint(15, 20)
        cls.initial_gen = True
        cls.pp = pprint.PrettyPrinter()

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        # Create a community for the new generation
        self.community = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']

    def tearDown(self):
        self.app_context.pop()

    def test_get_id(self):
        # Create a generation and test that the generation has the correct id
        try:
            generation = Generation(self.strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, [])
            self.assertEqual(0, generation.id)
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_create_start_point_less_than_end_point(self):
        # Try to create a generation with incorrect start end points, assert an exception is raised
        with self.assertRaises(GenerationCreationException):
            Generation(self.strategies, 0, self.community, 10, 8, self.num_of_onlookers, [])

    def test_create_start_point_equal_to_end_point(self):
        # Try to create a generation with incorrect start end points, assert an exception is raised
        with self.assertRaises(GenerationCreationException):
            Generation(self.strategies, 0, self.community, 10, 10, self.num_of_onlookers, [])

    def test_get_start_point(self):
        # Create generation, test we get back the correct start point
        try:
            generation = Generation(self.strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, [])
            self.assertEqual(self.start_point, generation.get_start_point())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_get_end_point(self):
        # Create generation, test we get back the correct start point
        try:
            generation = Generation(self.strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, [])
            self.assertEqual(self.end_point, generation.get_end_point())
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_get_onlookers(self):
        # Test that the generated onlookers contain the donor and recipient, and the correct number of other players
        try:
            generation = Generation(self.strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, [])
            donor_id = random.randint(0, len(generation.get_players())-1)
            recipient_id = random.randint(0, len(generation.get_players())-1)
            action = InteractionAction(0, donor_id, generation.id, "reason", recipient_id,
                                       InteractionContent.COOPERATE)
            onlookers = generation._generate_onlookers(action)
            self.assertEqual(self.num_of_onlookers+2, len(onlookers))
            self.assertTrue(donor_id in onlookers, self.initial_gen)
            self.assertTrue(recipient_id in onlookers, self.initial_gen)
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")

    def test_simulate(self):
        # Test the simulation of a community
        try:
            action_observer = ActionObserver(self.community, [0])
            generation = Generation(self.strategies, 0, self.community, self.start_point, self.end_point,
                                    self.num_of_onlookers, [action_observer])
            generation.simulate()
            num_of_players = 0
            for strategy in self.strategies:
                strat_count = self.strategies[strategy]
                num_of_players += strat_count
            actions = action_observer.actions
            timepoints = 0
            for timepoint in actions:
                timepoints += 1
                # Test that each player has committed to an action at every timepoint
                self.assertEqual(num_of_players, len(actions[timepoint]), "Should be the same number of players"
                                                                          " as actions in each timepoint")
            # Check that there is the correct number of timepoints simulated in the generation
            self.assertEqual(timepoints, self.end_point - self.start_point, "Should be an equal number of timepoints as"
                                                                            " endpoint-startpoint")
            percepts = {}
            for player in generation.get_players():
                for timepoint in player._percepts:
                    if timepoint in percepts:
                        for percept in player._percepts[timepoint]:
                            percepts[timepoint].append(percept)
                    else:
                        percepts[timepoint] = player._percepts[timepoint]
            # Check that there is perecepts for each timepoint
            self.assertEqual(self.end_point-self.start_point, len(percepts), "Should be a list of percepts for "
                                                                             "each timepoint")
            for timepoint in percepts:
                # Calculate number of percepts there should be
                num_of_expected_percepts = 0
                if timepoint in actions:
                    for action in actions[timepoint]:
                        if action.type is ActionType.GOSSIP:
                            num_of_expected_percepts += 1
                        elif action.type is ActionType.INTERACTION:
                            interaction: InteractionAction = action
                            num_of_expected_percepts += len(interaction.onlookers)
                # Check that there is the correct amount of percepts for each timepoint
                self.assertEqual(num_of_expected_percepts, len(percepts[timepoint]), "Should be one percept for each"
                                                                                     " gossip action, 2+num_of_onlooker"
                                                                                     " percepts for each interaction"
                                                                                     " action")
            print("Actions: ")
            self.pp.pprint(actions)
            print("Percepts: ")
            self.pp.pprint(percepts)
        except GenerationCreationException:
            self.fail("Should not have failed to create generation")
        except SimulationException:
            self.fail("Should not fail to simulate")

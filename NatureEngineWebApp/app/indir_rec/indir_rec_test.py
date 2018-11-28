"""indir_rec_test.py: Test the indirect reciprocity model implementation"""

__author__ = "James King adapted from Miguel Grinberg"

import unittest
from tests.test_config import TestConfig
from app import create_app
import requests
from flask import current_app
from .community_logic import Community, CommunityCreationException
from .generation_logic import Generation, GenerationCreationException
from .percept_logic import PerceptInteraction, PerceptAction, PerceptionException
from .player_logic import PlayerFactory, PlayerCreationException, Player
from .action_logic import Action, Gossip


# class CommunityTest(unittest.TestCase):
#
#     def setUp(self):
#         self.app = create_app(TestConfig)
#         self.app_context = self.app.app_context()
#         self.app_context.push()
#
#     def tearDown(self):
#         self.app_context.pop()
#
#     def test_create_community(self):
#         strategies = {"cooperator": 2, "defector": 2}
#         try:
#             Community(initial_strategies=strategies)
#         except CommunityCreationException:
#             self.fail("Shouldn't fail to create the community")
#
#     def test_create_generation(self):
#
#
# class GenerationCreationTest(unittest.TestCase):
#
#     def setUp(self):
#         self.app = create_app(TestConfig)
#         self.app_context = self.app.app_context()
#         self.app_context.push()
#         self.strategies = {"cooperator": 2, "defector": 2}
#         self.community = Community(initial_strategies=self.strategies)
#
#     def tearDown(self):
#         self.app_context.pop()
#
#     def test_create_generation_and_players(self):
#         strategies = {"cooperator": 2, "defector": 2}
#         try:
#             Generation(self.strategies, communityID=community.get_id(),
#                        start_time=0, end_time=10, id=0, onlooker_number=5)
#         except GenerationCreationException:
#             self.fail("Shouldn't fail to create the generation")

class PerceptTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community, "generation": 0}).json()
        self.generation = 0
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Standing Discriminator",
                                                          "options": ["distrusting"], "community": self.community,
                                                          "generation": 0, "player": 2}).json()
        self.standing_player = 2
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Defector",
                                                          "options": [], "community": self.community,
                                                          "generation": 0, "player": 0}).json()
        self.defector_player = 0
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Cooperator",
                                                          "options": [], "community": self.community,
                                                          "generation": 0, "player": 1}).json()
        self.coop_player = 1

    def tearDown(self):
        self.app_context.pop()

    def test_send_gossip(self):
        percept = PerceptAction(self.standing_player, 4, {"type": "gossip", "about": self.defector_player,
                                                          "gossiper": self.coop_player, "gossip": "positive",
                                                          "timepoint": 3})
        try:
            percept.perceive(self.community, self.generation)
        except PerceptionException as e:
            print(e)
            self.fail("Should not fail to perceive")

    def test_send_action(self):
        percept = PerceptAction(self.coop_player, 7, {"type": "interaction", "donor": self.defector_player,
                                                      "recipient": self.standing_player, "action": "defect",
                                                      "timepoint": 6})
        try:
            percept.perceive(self.community, self.generation)
        except PerceptionException as e:
            print(e)
            self.fail("Should not fail to perceive the action")

    def test_send_interaction(self):
        percept = PerceptInteraction(self.coop_player, self.defector_player, 8)
        try:
            percept.perceive(self.community, self.generation)
        except PerceptionException as e:
            print(e)
            self.fail("Should not fail to perceive the interaction")

    def test_cause_fail(self):
        percept = PerceptInteraction(self.coop_player, self.defector_player, 8)
        with self.assertRaises(PerceptionException):
            percept.perceive(self.community+1000, self.generation)


class PlayerFactoryTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community,
                                                                                       "generation": 0}).json()
        self.generation = 0

    def tearDown(self):
        self.app_context.pop()

    def test_create_all_strategies(self):
        strategies = requests.request("GET", current_app.config['AGENTS_URL'] + 'strategies').json()['strategies']
        id = 0
        for strategy in strategies:
            try:
                player = PlayerFactory.new_player(strategy['name'], strategy['options'],
                                                  self.community, self.generation, id)
                self.assertEqual(type(player), Player, msg="Should have created a player")
            except PlayerCreationException as e:
                print(e)
                self.fail("Should have created the player correctly")
            id += 1

    def test_cause_fail(self):
        with self.assertRaises(PlayerCreationException):
            PlayerFactory.new_player("unknown strategy", [],
                                     self.community, self.generation, 0)


# class ActionTest(unittest.TestCase):
#
#     def setUp(self):
#         self.app = create_app(TestConfig)
#         self.app_context = self.app.app_context()
#         self.app_context.push()
#         self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
#         requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community, "generation": 0}).json()
#         self.generation = 0
#         requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Standing Discriminator",
#                                                           "options": ["distrusting"], "community": self.community,
#                                                           "generation": 0, "player": 2}).json()
#         self.standing_player = 2
#         requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Defector",
#                                                           "options": [], "community": self.community,
#                                                           "generation": 0, "player": 0}).json()
#         self.defector_player = 0
#         requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Cooperator",
#                                                           "options": [], "community": self.community,
#                                                           "generation": 0, "player": 1}).json()
#         self.coop_player = 1
#
#     def tearDown(self):
#         self.app_context.pop()
#
#     def test_gossip_type(self):







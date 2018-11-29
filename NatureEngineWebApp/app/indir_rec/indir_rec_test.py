"""indir_rec_test.py: Test the indirect reciprocity model implementation"""

__author__ = "James King adapted from Miguel Grinberg"

import unittest
from tests.test_config import TestConfig
from app import create_app
import requests
import random
from flask import current_app
from .community_logic import Community, CommunityCreationException
from .generation_logic import Generation, GenerationCreationException
from .percept_logic import PerceptInteraction, PerceptAction, PerceptionException
from .player_logic import PlayerFactory, PlayerCreationException, Player, DecisionException
from .action_logic import Interaction, Gossip


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


def interacting_this_turn(timepoint, player, interactions):
    for interaction in interactions:
        if interaction['timepoint'] == timepoint and interaction['donor'] == player:
            return True
    return False


class PlayerTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community,
                                                                                       "generation": 0}).json()
        self.generation = 0
        self.player = PlayerFactory.new_player("Standing Discriminator", ["trusting"],
                                               self.community, self.generation, 2)
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Defector",
                                                                                  "options": [],
                                                                                  "community": self.community,
                                                                                  "generation": 0, "player": 0}).json()
        self.defector_player = 0
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'agent', json={"strategy": "Cooperator",
                                                                                  "options": [],
                                                                                  "community": self.community,
                                                                                  "generation": 0, "player": 1}).json()
        self.coop_player = 1
        self.interactions = [{'timepoint': 1, 'donor': self.player.get_id(),
                              'recipient': self.coop_player},
                             {'timepoint': 4, 'donor': self.player.get_id(),
                              'recipient': self.defector_player},
                             {'timepoint': 6, 'donor': self.player.get_id(),
                              'recipient': self.coop_player},
                             {'timepoint': 7, 'donor': self.player.get_id(),
                              'recipient': self.defector_player},
                             {'timepoint': 11, 'donor': self.player.get_id(),
                              'recipient': self.coop_player}]
        for interaction in self.interactions:
            percept_payload = {"community": self.community, "generation": self.generation,
                               "donor": interaction['donor'], "recipient": interaction['recipient'],
                               "timepoint": interaction['timepoint']}
            self.percept_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/interaction',
                                                     json=percept_payload).json()
        self.gossip_percepts = [{"perceiver": self.player.get_id(), "about": self.defector_player,
                                 "gossiper": self.coop_player, "timepoint": 2, "gossip": "positive"},
                                {"perceiver": self.player.get_id(), "about": self.coop_player,
                                 "gossiper": self.defector_player, "timepoint": 3, "gossip": "negative"},
                                {"perceiver": self.player.get_id(), "about": self.defector_player,
                                 "gossiper": self.coop_player, "timepoint": 6, "gossip": "negative"}]
        for gossip in self.gossip_percepts:
            gossip_payload = {"community": self.community, "generation": self.generation,
                              "perceiver": gossip['perceiver'], "about": gossip['about'],
                              "gossiper": gossip['gossiper'], "timepoint": gossip['timepoint'],
                              "gossip": gossip['gossip']}
            self.gossip_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/action/gossip',
                                                     json=gossip_payload).json()
        self.action_percepts = [{"perceiver": self.player.get_id(), "donor": self.defector_player,
                                 "recipient": self.coop_player, "timepoint": 2, "action": "defect"},
                                {"perceiver": self.player.get_id(), "donor": self.coop_player,
                                 "recipient": self.defector_player, "timepoint": 5, "action": "cooperate"}]
        for action in self.action_percepts:
            action_payload = {"community": self.community, "generation": self.generation,
                              "perceiver": action['perceiver'], "donor": action['donor'],
                              "recipient": action['recipient'], "timepoint": action['timepoint'],
                              "action": action['action']}
            self.action_payload = requests.request("POST", current_app.config['AGENTS_URL'] +
                                                   'percept/action/interaction', json=action_payload).json()

    def tearDown(self):
        self.app_context.pop()

    def test_get_id(self):
        self.assertEqual(2, self.player.get_id(), msg="Id should be the same as assigned")
        self.assertNotEqual(1,  self.player.get_id(), msg="Id should be the same as assigned")

    def test_get_strategy(self):
        self.assertEqual({'name': "Standing Discriminator", 'options': ['trusting']},
                         self.player.get_strategy())

    def test_fitness(self):
        self.assertEqual(0, self.player.get_fitness(), msg="Fitness should start on 0")
        self.player.update_fitness(-2)
        self.assertEqual(0, self.player.get_fitness(), msg="Fitness should not go below zero")
        self.player.update_fitness(20)
        self.assertEqual(20, self.player.get_fitness(), msg="Fitness should have increased by 20")
        self.player.update_fitness(-3)
        self.assertEqual(17, self.player.get_fitness(), msg="Fitness should have decreased by 3")
        self.player.update_fitness(-20)
        self.assertEqual(0, self.player.get_fitness(), msg="Fitness should not go below zero")

    def test_decide(self):
        for timepoint in range(20):
            try:
                action = self.player.decide(timepoint)
                print("timepoint: " + str(timepoint) + ", action: " + str(action))
                if interacting_this_turn(timepoint, self.player.get_id(), self.interactions):
                    self.assertEqual("action", action['type'])
                    standing_belief_payload = {"timepoint": timepoint, "community": self.community,
                                               "generation": 0, "perceiver": self.player.get_id(),
                                               "about": action['recipient']}
                    standing_belief_response = requests.request("GET", current_app.config['AGENTS_URL'] +
                                                                'belief/standing',
                                                                params=standing_belief_payload).json()
                    if standing_belief_response['standing'] == "good":
                        self.assertEqual("cooperate", action['value'])
                    elif standing_belief_response['standing'] == "bad":
                        self.assertEqual("defect", action['value'])
                    else:
                        self.fail(msg="Incorrect standing")
            except DecisionException as e:
                print(e)
                self.fail("Should not have failed to make a decision")


class InteractionTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community,
                                                                                       "generation": 0}).json()
        self.generation = 0
        strategies = requests.request("GET", current_app.config['AGENTS_URL'] + 'strategies').json()['strategies']
        self.players = []
        id = 0
        for strategy in strategies:
            self.players.append(PlayerFactory.new_player(strategy['name'], strategy['options'],
                                                         self.community, self.generation, id))
            id += 1

    def tearDown(self):
        self.app_context.pop()

    def test_get_type(self):
        interaction = Interaction(3, random.choice(self.players), random.choice(self.players), "defect")
        self.assertEqual("interaction", interaction.get_type())

    def test_get_action(self):
        donor = random.choice(self.players)
        recipient = random.choice(self.players)
        action = "cooperate"
        timepoint = 7
        onlookers = []
        interaction = Interaction(timepoint, donor, recipient, action)
        self.assertEqual({"timepoint": timepoint, "donor": donor, "recipient": recipient, "onlookers": onlookers,
                          "type": "interaction"},
                         interaction.get_action())

    def test_update_onlookers(self):
        donor = random.choice(self.players)
        recipient = random.choice(self.players)
        action = "cooperate"
        timepoint = 7
        onlookers1 = []
        interaction = Interaction(timepoint, donor, recipient, action)
        for i in range(2):
            onlookers1.append(random.choice(self.players))
        interaction.update_onlookers(onlookers1)
        self.assertEqual({"timepoint": timepoint, "donor": donor, "recipient": recipient, "onlookers": onlookers1,
                          "type": "interaction"},
                         interaction.get_action())
        onlookers2 = []
        for i in range(3):
            onlookers2.append(random.choice(self.players))
        interaction.update_onlookers(onlookers2)
        self.assertEqual({"timepoint": timepoint, "donor": donor, "recipient": recipient,
                          "onlookers": onlookers1 + onlookers2, "type": "interaction"}, interaction.get_action())

    def test_execute_cooperate(self):
        donor = random.choice(self.players)
        recipient = random.choice(self.players)
        action = "cooperate"
        timepoint = 7
        onlookers = [random.choice(self.players), random.choice(self.players)]
        interaction = Interaction(timepoint, donor, recipient, action)
        interaction.update_onlookers(onlookers)
        perceivers = [donor.get_id(), recipient.get_id()]
        perceivers.extend([onlooker.get_id() for onlooker in onlookers])
        percepts = interaction.execute()
        self.assertEqual(0, donor.get_fitness(), msg="Should not change as already 0")
        self.assertEqual(2, recipient.get_fitness(), msg="Should have gained 2 from cooperation")
        for percept in percepts:
            print(percept.get_perceiver())
            print(perceivers)
            self.assertTrue(percept.get_perceiver() in perceivers)
            perceivers.remove(percept.get_perceiver())
            try:
                percept.perceive(self.community, self.generation)
            except PerceptionException as e:
                print(e)
                self.fail("Should not fail to perceive the interaction")
        interaction2 = Interaction(8, recipient, donor, action)
        interaction2.update_onlookers(onlookers)
        perceivers2 = [donor.get_id(), recipient.get_id()]
        perceivers2.extend([onlooker.get_id() for onlooker in onlookers])
        percepts2 = interaction2.execute()
        self.assertEqual(2, donor.get_fitness(), msg="Should have gained 2 from cooperation")
        self.assertEqual(1, recipient.get_fitness(), msg="Should have lost 1 from cooperating")
        for percept in percepts2:
            self.assertTrue(percept.get_perceiver() in perceivers2)
            perceivers2.remove(percept.get_perceiver())
            try:
                percept.perceive(self.community, self.generation)
            except PerceptionException as e:
                print(e)
                self.fail("Should not fail to perceive the interaction")


class GossipTest(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
        requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community,
                                                                                       "generation": 0}).json()
        self.generation = 0
        strategies = requests.request("GET", current_app.config['AGENTS_URL'] + 'strategies').json()['strategies']
        self.players = []
        id = 0
        for strategy in strategies:
            self.players.append(PlayerFactory.new_player(strategy['name'], strategy['options'],
                                                         self.community, self.generation, id))
            id += 1

    def tearDown(self):
        self.app_context.pop()

    def test_get_type(self):
        gossip = Gossip(3, random.choice(self.players), random.choice(self.players),
                        random.choice(self.players), "positive")
        self.assertEqual("gossip", gossip.get_type())

    def test_get_action(self):
        gossip_action = "positive"
        about = random.choice(self.players)
        gossiper = random.choice(self.players)
        recipient = random.choice(self.players)
        timepoint = 1
        gossip = Gossip(timepoint, about, gossiper, recipient, gossip_action)
        self.assertEqual({"gossip": gossip_action, "about": about, "gossiper": gossiper, "recipient": recipient,
                          "timepoint": timepoint, "type": "gossip"}, gossip.get_action())

    def test_execute(self):
        gossip_action = "positive"
        about = random.choice(self.players)
        gossiper = random.choice(self.players)
        recipient = random.choice(self.players)
        timepoint = 1
        gossip = Gossip(timepoint, about, gossiper, recipient, gossip_action)
        percepts = gossip.execute()
        self.assertEqual(1, len(percepts))
        self.assertEqual(PerceptAction(recipient.get_id(), timepoint, gossip.get_action()).get_percept(),
                         percepts[0].get_percept())


# class GenerationTest(unittest.TestCase):
#
#     def setUp(self):
#         self.app = create_app(TestConfig)
#         self.app_context = self.app.app_context()
#         self.app_context.push()
#         self.community = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
#         requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation', json={"community": self.community,
#                                                                                        "generation": 0}).json()
#         self.generation = 0
#         strategies = requests.request("GET", current_app.config['AGENTS_URL'] + 'strategies').json()['strategies']
#         self.generation_strategies = []
#         id = 0
#         for strategy in strategies:
#             self.generation_strategies.append({strategy['name']})
#             id += 1



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







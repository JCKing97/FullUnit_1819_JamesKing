"""action_tests.py: Testing for the logic contained in action_logic.py"""

import random
import unittest
from .action_logic import ActionType, IdleAction, GossipAction, GossipContent, InteractionAction, InteractionContent


class IdleTests(unittest.TestCase):

    def setUp(self):
        self.actor = random.randint(0, 100)
        self.timepoint = random.randint(0, 100)
        self.generation = random.randint(0, 100)
        self.action = IdleAction(self.timepoint, self.actor, self.generation)

    def test_get_generation(self):
        self.assertEqual(self.generation, self.action.generation, "Should have been the same as set")

    def test_get_actor(self):
        self.assertEqual(self.actor, self.action.actor, "Should have the same property as the set actor")

    def test_get_timepoint(self):
        self.assertEqual(self.timepoint, self.action.timepoint, "Should have the same property as the set timepoint")

    def test_get_type(self):
        self.assertEqual(ActionType.IDLE, self.action.type, "Should be the IDLE type")


class GossipTests(unittest.TestCase):

    def setUp(self):
        self.gossiper = random.randint(0, 100)
        self.recipient = random.randint(0, 100)
        self.about = random.randint(0, 100)
        self.timepoint = random.randint(0, 100)
        self.generation = random.randint(0, 100)
        self.gossip = random.choice(list(GossipContent))
        self.action = GossipAction(self.timepoint, self.gossiper, self.generation, self.about, self.recipient,
                                   self.gossip)

    def test_get_generation(self):
        self.assertEqual(self.generation, self.action.generation, "Should have been the same as set")

    def test_get_actor(self):
        self.assertEqual(self.gossiper, self.action.actor, "Should have the same property as the set gossiper")

    def test_get_gossiper(self):
        self.assertEqual(self.gossiper, self.action.gossiper, "Should have the same property as the set gossiper")

    def test_get_timepoint(self):
        self.assertEqual(self.timepoint, self.action.timepoint, "Should have the same property as the set timepoint")

    def test_get_type(self):
        self.assertEqual(ActionType.GOSSIP, self.action.type, "Should be the GOSSIP type")

    def test_get_about(self):
        self.assertEqual(self.about, self.action.about, "Should be the same as the set about")

    def test_get_recipient(self):
        self.assertEqual(self.recipient, self.action.recipient, "Should be the same as the set recipient")

    def test_get_gossip(self):
        self.assertEqual(self.gossip, self.action.gossip, "Should be the same as the set gossip")


class InteractionTests(unittest.TestCase):

    def setUp(self):
        self.donor = random.randint(0, 100)
        self.recipient = random.randint(0, 100)
        self.timepoint = random.randint(0, 100)
        self.interaction_content = random.choice(list(InteractionContent))
        self.generation = random.randint(0, 100)
        self.action = InteractionAction(self.timepoint, self.donor, self.generation, self.recipient,
                                        self.interaction_content)

    def test_get_generation(self):
        self.assertEqual(self.generation, self.action.generation, "Should have been the same as set")

    def test_get_actor(self):
        self.assertEqual(self.donor, self.action.actor, "Should have the same property as the set donor")

    def test_get_donor(self):
        self.assertEqual(self.donor, self.action.donor, "Should have the same property as the set donor")

    def test_get_timepoint(self):
        self.assertEqual(self.timepoint, self.action.timepoint, "Should have the same property as the set timepoint")

    def test_get_type(self):
        self.assertEqual(ActionType.INTERACTION, self.action.type, "Should be the DONOR type")

    def test_get_recipient(self):
        self.assertEqual(self.recipient, self.action.recipient, "Should be the same as the set recipient")

    def test_get_action(self):
        self.assertEqual(self.interaction_content, self.action.action, "Should be the same action value")

    def test_onlookers(self):
        self.assertEqual([], self.action.onlookers, "onlookers should be empty")
        onlookers = []
        for i in range(0, 15):
            onlookers.append(random.randint(0, 100))
        self.action.onlookers = onlookers
        self.assertEqual(onlookers, self.action.onlookers, "onlookers should have been set to the array built")

    def test_onlookers_constructor(self):
        onlookers = []
        for i in range(0, 15):
            onlookers.append(random.randint(0, 100))
        new_action = InteractionAction(self.timepoint, self.donor, self.recipient, self.interaction_content, onlookers)
        self.assertEqual(onlookers, new_action.onlookers, "onlookers should have been set to the array built")




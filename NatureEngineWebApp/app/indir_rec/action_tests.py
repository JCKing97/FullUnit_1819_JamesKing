"""action_tests.py: Testing for the logic contained in action_logic.py"""

import random
import unittest
from .action_logic import ActionType, IdleAction, GossipAction, GossipContent, InteractionAction, InteractionContent


class IdleTests(unittest.TestCase):
    """Test the IdleAction class"""

    def setUp(self):
        # Create the required resources (actor id, timepoint, generation and action) to test with
        self.actor = random.randint(0, 100)
        self.timepoint = random.randint(0, 100)
        self.generation = random.randint(0, 100)
        self.action = IdleAction(self.timepoint, self.actor, self.generation, "reason")

    def test_get_generation(self):
        # Test that the get generation property of the action is the same as the one set
        self.assertEqual(self.generation, self.action.generation, "Should have been the same as set")

    def test_get_actor(self):
        # Test that the getter for the actor that committed the action is the same as set
        self.assertEqual(self.actor, self.action.actor, "Should have the same property as the set actor")

    def test_get_timepoint(self):
        # Test that the return from the getter of the timepoint of the action is the same as teh timepoint set
        self.assertEqual(self.timepoint, self.action.timepoint, "Should have the same property as the set timepoint")

    def test_get_type(self):
        # Test that the action returns the type IDLE
        self.assertEqual(ActionType.IDLE, self.action.type, "Should be the IDLE type")


class GossipTests(unittest.TestCase):
    """Test the GossipAction class"""

    def setUp(self):
        # Set up the relevant resources (gossip, recipient and about ids, timepoint, generation, gossip content and
        # action) to tets the class
        self.gossiper = random.randint(0, 100)
        self.recipient = random.randint(0, 100)
        self.about = random.randint(0, 100)
        self.timepoint = random.randint(0, 100)
        self.generation = random.randint(0, 100)
        self.gossip = random.choice(list(GossipContent))
        self.action = GossipAction(self.timepoint, self.gossiper, self.generation, "reason", self.about, self.recipient,
                                   self.gossip)

    def test_get_generation(self):
        # Test that the getter for the generation property returns the correct generation
        self.assertEqual(self.generation, self.action.generation, "Should have been the same as set")

    def test_get_actor(self):
        # Test that the getter for the actor property returns the gossiper
        self.assertEqual(self.gossiper, self.action.actor, "Should have the same property as the set gossiper")

    def test_get_gossiper(self):
        # Test that the getter for the gossiper property returns the correct gossiper
        self.assertEqual(self.gossiper, self.action.gossiper, "Should have the same property as the set gossiper")

    def test_get_timepoint(self):
        # Test that the timepoint getter is gets the set timepoint
        self.assertEqual(self.timepoint, self.action.timepoint, "Should have the same property as the set timepoint")

    def test_get_type(self):
        # Test that the correct type is returned (GOSSIP)
        self.assertEqual(ActionType.GOSSIP, self.action.type, "Should be the GOSSIP type")

    def test_get_about(self):
        # Test that the correct id for the about is returned by the about getter property
        self.assertEqual(self.about, self.action.about, "Should be the same as the set about")

    def test_get_recipient(self):
        # Test that the correct id for the recipient is returned by the recipient getter property
        self.assertEqual(self.recipient, self.action.recipient, "Should be the same as the set recipient")

    def test_get_gossip(self):
        # Test that the set gossip content is returned correctly
        self.assertEqual(self.gossip, self.action.gossip, "Should be the same as the set gossip")


class InteractionTests(unittest.TestCase):
    """Test the InteractionAction class"""

    def setUp(self):
        # Set up the relevant resources (donor and recipient ids, timepoint, generation, interaction content and
        # action) required for testing the action
        self.donor = random.randint(0, 100)
        self.recipient = random.randint(0, 100)
        self.timepoint = random.randint(0, 100)
        self.interaction_content = random.choice(list(InteractionContent))
        self.generation = random.randint(0, 100)
        self.action = InteractionAction(self.timepoint, self.donor, self.generation, "reason",
                                        self.recipient, self.interaction_content)

    def test_get_generation(self):
        # Test that the set generation is correctly returned by getter property of generation
        self.assertEqual(self.generation, self.action.generation, "Should have been the same as set")

    def test_get_actor(self):
        # Test that the correct id for the donor is returned as the actor property
        self.assertEqual(self.donor, self.action.actor, "Should have the same property as the set donor")

    def test_get_donor(self):
        # Test that the correct id for the donor is returned as the donor property
        self.assertEqual(self.donor, self.action.donor, "Should have the same property as the set donor")

    def test_get_timepoint(self):
        # Test that the set timepoint is correctly returned
        self.assertEqual(self.timepoint, self.action.timepoint, "Should have the same property as the set timepoint")

    def test_get_type(self):
        # Test that the action correctly returns the ActionType INTERACTION
        self.assertEqual(ActionType.INTERACTION, self.action.type, "Should be the INTERACTION type")

    def test_get_recipient(self):
        # Tests that the set recipient id is correctly returned by the recipient property getter
        self.assertEqual(self.recipient, self.action.recipient, "Should be the same as the set recipient")

    def test_get_action(self):
        # Tests that the correct interaction content is returned by the action getter property
        self.assertEqual(self.interaction_content, self.action.action, "Should be the same action value")

    def test_onlookers(self):
        # Test that the onlookers (not set in the constructor) are originally empty
        # Then setting the onlookers using the setter method and testing that they are correct
        self.assertEqual([], self.action.onlookers, "onlookers should be empty")
        onlookers = []
        for i in range(0, 15):
            onlookers.append(random.randint(0, 100))
        self.action.onlookers = onlookers
        self.assertEqual(onlookers, self.action.onlookers, "onlookers should have been set to the array built")

    def test_onlookers_constructor(self):
        # Test the setting of onlookers in the constructor is the correctly returned in the onlookers property getter
        onlookers = []
        for i in range(0, 15):
            onlookers.append(random.randint(0, 100))
        new_action = InteractionAction(self.timepoint, self.donor, self.generation, "reason", self.recipient,
                                       self.interaction_content, onlookers)
        self.assertEqual(onlookers, new_action.onlookers, "onlookers should have been set to the array built")




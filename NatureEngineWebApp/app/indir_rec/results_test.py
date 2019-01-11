"""results_test.py: Testing for the logic contained in results_logic.py"""

__author__ = "James King"

import unittest
import random
from .results_logic import ActionObserver, RecordingError
from .action_logic import IdleAction


class ActionObserverTest(unittest.TestCase):

    def setUp(self):
        self.community = random.randint(0, 100)
        self.action_observer = ActionObserver(self.community)

    def test_get_corrupted_observations(self):
        self.assertFalse(self.action_observer.corrupted_observations)

    def test_get_community(self):
        self.assertEqual(self.community, self.action_observer.community, "Should be the same as the set comumnity")

    def test_get_generation(self):
        self.assertEqual([], self.action_observer.generations, "Should be set as empty")

    def test_add_generation(self):
        self.action_observer.add_generation(2)
        self.assertEqual([2], self.action_observer.generations, "Should have added 2")
        self.action_observer.add_generation(19)
        self.assertEqual([2, 19], self.action_observer.generations, "Should have added 19")
        self.action_observer.add_generation(4)
        self.assertEqual([2, 19, 4], self.action_observer.generations, "Should have added 4")
        self.assertFalse(self.action_observer.corrupted_observations)
        self.assertEqual({2: [], 19: [], 4: []}, self.action_observer.players,
                         "Should have added an entry in the players dict for every generation")
        with self.assertRaises(RecordingError):
            self.action_observer.add_generation(19)
        self.assertEqual([2, 19, 4], self.action_observer.generations, "Should have failed to add")
        self.assertTrue(self.action_observer.corrupted_observations)

    def test_generation_in_constructor(self):
        generations = [23, 5, 78, 29, 9]
        new_action_observer = ActionObserver(self.community, generations)
        self.assertEqual(generations, new_action_observer.generations, "Should have the same generations as set in"
                                                                       " constructor")
        self.assertEqual({23: [], 5: [], 78: [], 29: [], 9: []}, new_action_observer.players,
                         "Should have added an entry in the players dict for every generation")

    def test_generations_in_constructor_with_identical(self):
        generations = [23, 5, 78, 23, 9, 5]
        with self.assertRaises(RecordingError):
            new_action_observer = ActionObserver(self.community, generations)
            self.assertEqual(self.community, new_action_observer.community, "Should be the same as the set comumnity")
            self.assertTrue(new_action_observer.corrupted_observations)

    def test_get_players(self):
        self.assertEqual({}, self.action_observer.players, "Should be empty with no generations")

    def test_add_players(self):
        # add generations
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_generation(78)
        # add players
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        self.assertEqual({2: [8, 9, 12], 6: [8, 9], 78: []}, self.action_observer.players, "Should have added the"
                                                                                           " correct gens and players")

    def test_add_players_to_non_existent_gen(self):
        with self.assertRaises(RecordingError):
            self.action_observer.add_player(6, 34)

    def test_add_identical_generations(self):
        self.action_observer.add_generation(2)
        self.action_observer.add_player(2, 8)
        with self.assertRaises(RecordingError):
            self.action_observer.add_player(2, 8)

    def test_get_actions(self):
        self.assertEqual([], self.action_observer.actions, "Should contain no actions at the start")

    def test_update_action(self):
        self.action_observer.add_generation(5)
        self.action_observer.add_player(5, 7)
        action = IdleAction(3, 5, 7)
        self.action_observer.update(action)
        self.assertEqual([action], self.action_observer.actions, "Should contain the one action that has been added")

    def test_update_action_non_existent_player(self):
        self.action_observer.add_generation(5)
        action = IdleAction(3, 5, 7)
        with self.assertRaises(RecordingError):
            self.action_observer.update(action)
        self.assertEqual([], self.action_observer.actions, "Should contain no actions as non-existent player")

    def test_update_action_non_existent_generation(self):
        action = IdleAction(3, 5, 7)
        with self.assertRaises(RecordingError):
            self.action_observer.update(action)
        self.assertEqual([], self.action_observer.actions, "Should contain no actions as non-existent generation")








"""observation_test.py: Testing for the logic contained in observation_logic.py"""

__author__ = "James King"

import unittest
import random
from .observation_logic import ActionObserver, RecordingError
from .action_logic import IdleAction, Action
from typing import List
from .player_logic import PlayerState


class MockPlayerState(PlayerState):

    @property
    def new_action(self) -> Action:
        return self._new_action

    @new_action.setter
    def new_action(self, action: Action) -> None:
        self._new_action = action

    @property
    def fitness_update(self) -> int:
        return self._fitness_update

    @fitness_update.setter
    def fitness_update(self, fitness: int):
        self._fitness_update = fitness


class ActionObserverTest(unittest.TestCase):

    def setUp(self):
        self.community = random.randint(0, 100)
        self.action_observer = ActionObserver(self.community)
        self.generation = 0
        self.player = 0
        self.player_state = MockPlayerState(self.generation, self.player, [self.action_observer])

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
        self.assertEqual({2: [8, 9, 12], 6: [8, 34], 78: []}, self.action_observer.players, "Should have added the"
                                                                                            " correct gens and players")

    def test_add_players_to_non_existent_gen(self):
        with self.assertRaises(RecordingError):
            self.action_observer.add_player(6, 34)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_add_identical_players(self):
        self.action_observer.add_generation(2)
        self.action_observer.add_player(2, 8)
        with self.assertRaises(RecordingError):
            self.action_observer.add_player(2, 8)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_get_actions(self):
        self.assertEqual([], self.action_observer.actions, "Should contain no actions at the start")

    def test_update_action(self):
        self.action_observer.add_generation(self.generation)
        self.action_observer.add_player(self.generation, self.player)
        action = IdleAction(3, self.generation, self.player)
        self.player_state.attach(self.action_observer)
        self.player_state.new_action = action
        self.action_observer.update(self.player_state)
        self.assertEqual([action], self.action_observer.actions, "Should contain the one action that has been added")

    def test_update_action_non_existent_player(self):
        self.action_observer.add_generation(5)
        action = IdleAction(3, 5, 7)
        self.player_state.new_action = action
        with self.assertRaises(RecordingError):
            self.action_observer.update(self.player_state)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded that observations"
                                                                     "may be corrupted")
        self.assertEqual([], self.action_observer.actions, "Should contain no actions as non-existent player")

    def test_update_action_non_existent_generation(self):
        action = IdleAction(3, 5, 7)
        self.player_state.new_action = action
        with self.assertRaises(RecordingError):
            self.action_observer.update(self.player_state)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded that observations"
                                                                     "may be corrupted")
        self.assertEqual([], self.action_observer.actions, "Should contain no actions as non-existent generation")

    def test_initial_cooperation_rate(self):
        self.assertEqual(0, self.action_observer.cooperation_rate)
        self.assertEqual({}, self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({}, self.action_observer.cooperation_rate_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        self.assertEqual({2: 0, 6: 0},self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({2: {8: 0, 9: 0, 12: 0}, 6: {8: 0, 34: 0}}, self.action_observer.cooperation_rate_by_generation_and_player)









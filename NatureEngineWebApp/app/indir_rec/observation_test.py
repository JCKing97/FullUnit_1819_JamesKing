"""observation_test.py: Testing for the logic contained in observation_logic.py"""

__author__ = "James King"

import unittest
import random
from .observation_logic import ActionObserver, RecordingError, PlayerObserver
from .action_logic import IdleAction, Action, InteractionAction, InteractionContent, GossipAction, GossipContent,\
    ActionType
from .player_logic import PlayerState, Player
from app import create_app
from tests.test_config import TestConfig
import requests
from .indir_rec_config import Config
from typing import Dict, List
from .strategy_logic import Strategy


class MockPlayerState(PlayerState):
    """A mock object of a players state to use"""

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
    """Test the ActionObserver class"""

    def setUp(self):
        # Create the relevant resources (community id, observer, generation and player ids and the mock player state
        # to observe
        self.community = random.randint(0, 100)
        self.action_observer = ActionObserver(self.community)
        self.generation = 0
        self.player = 0
        self.player_state = MockPlayerState(self.generation, self.player, [self.action_observer])

    def test_get_corrupted_observations(self):
        # Observations should not be corrupted at the beginning
        self.assertFalse(self.action_observer.corrupted_observations)

    def test_get_community(self):
        # Test that the community has been set and returns correctly
        self.assertEqual(self.community, self.action_observer.community, "Should be the same as the set community")

    def test_get_generation(self):
        # Test that no generation ids have been set yet
        self.assertEqual([], self.action_observer.generations, "Should be set as empty")

    def test_add_generation(self):
        # Add a generation and test that the relevant data structures have been updated
        self.action_observer.add_generation(2)
        self.assertEqual([2], self.action_observer.generations, "Should have added 2")
        self.action_observer.add_generation(19)
        self.assertEqual([2, 19], self.action_observer.generations, "Should have added 19")
        self.action_observer.add_generation(4)
        self.assertEqual([2, 19, 4], self.action_observer.generations, "Should have added 4")
        self.assertFalse(self.action_observer.corrupted_observations)
        self.assertEqual({2: [], 19: [], 4: []}, self.action_observer.players,
                         "Should have added an entry in the players dict for every generation")
        # Check that adding a duplicated generation causes corruption to be observed
        with self.assertRaises(RecordingError):
            self.action_observer.add_generation(19)
        self.assertEqual([2, 19, 4], self.action_observer.generations, "Should have failed to add")
        self.assertTrue(self.action_observer.corrupted_observations)

    def test_generation_in_constructor(self):
        # Add generations in the constructor and check that the relevant data structures have been updated
        generations = [23, 5, 78, 29, 9]
        new_action_observer = ActionObserver(self.community, generations)
        self.assertEqual(generations, new_action_observer.generations, "Should have the same generations as set in"
                                                                       " constructor")
        self.assertEqual({23: [], 5: [], 78: [], 29: [], 9: []}, new_action_observer.players,
                         "Should have added an entry in the players dict for every generation")

    def test_generations_in_constructor_with_identical(self):
        # Test the adding of identical generations in the constructor detects corrupted observations
        generations = [23, 5, 78, 23, 9, 5]
        with self.assertRaises(RecordingError):
            new_action_observer = ActionObserver(self.community, generations)
            self.assertEqual(self.community, new_action_observer.community, "Should be the same as the set comumnity")
            self.assertTrue(new_action_observer.corrupted_observations)

    def test_get_players(self):
        # Check players set is empty at the beginning
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
        # Check players have been added correctly
        self.assertEqual({2: [8, 9, 12], 6: [8, 34], 78: []}, self.action_observer.players, "Should have added the"
                                                                                            " correct gens and players")

    def test_add_players_to_non_existent_gen(self):
        # Check observation corruption has been found
        with self.assertRaises(RecordingError):
            self.action_observer.add_player(6, 34)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_add_identical_players(self):
        self.action_observer.add_generation(2)
        self.action_observer.add_player(2, 8)
        # Check observation corruption has been found
        with self.assertRaises(RecordingError):
            self.action_observer.add_player(2, 8)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_get_actions(self):
        # Check actions are empty at beginning
        self.assertEqual({}, self.action_observer.actions, "Should contain no actions at the start")

    def test_update_action(self):
        # Test attaching observer and updating with a new action for the create gen and player creates the relevant
        # entries in data structures
        self.action_observer.add_generation(self.generation)
        self.action_observer.add_player(self.generation, self.player)
        action = IdleAction(3, self.generation, self.player, "reason")
        self.player_state.attach(self.action_observer)
        self.player_state.new_action = action
        self.action_observer.update(self.player_state)
        self.assertEqual({action.timepoint: [action]}, self.action_observer.actions,
                         "Should contain the one action that has been added")

    def test_update_action_non_existent_player(self):
        # Attempt to update action of non-existent player
        self.action_observer.add_generation(5)
        action = IdleAction(3, 5, 7, "reason")
        self.player_state.new_action = action
        # Check observation corruption has been found
        with self.assertRaises(RecordingError):
            self.action_observer.update(self.player_state)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded that observations"
                                                                     "may be corrupted")
        self.assertEqual({}, self.action_observer.actions, "Should contain no actions as non-existent player")

    def test_update_action_non_existent_generation(self):
        # Attempt to update action of non-existent generation
        action = IdleAction(3, 5, 7, "reason")
        self.player_state.new_action = action
        # Check observation corruption has been found
        with self.assertRaises(RecordingError):
            self.action_observer.update(self.player_state)
        self.assertTrue(self.action_observer.corrupted_observations, "Should have recorded that observations"
                                                                     "may be corrupted")
        self.assertEqual({}, self.action_observer.actions, "Should contain no actions as non-existent generation")

    def test_initial_actions(self):
        # Test that data structures have been correctly initialised
        self.assertEqual({}, self.action_observer.actions)
        self.assertEqual({}, self.action_observer.actions_by_generation)
        self.assertEqual({}, self.action_observer.actions_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        # Check relevant data structures have been updated
        self.assertEqual({2: {}, 6: {}}, self.action_observer.actions_by_generation)
        self.assertEqual({2: {8: {}, 9: {}, 12: {}}, 6: {8: {}, 34: {}}},
                         self.action_observer.actions_by_generation_and_player)

    def test_add_actions(self):
        # Create relevant generations, players and player states for those players
        self.action_observer.add_generation(7)
        self.action_observer.add_generation(8)
        self.action_observer.add_player(7, 8)
        self.action_observer.add_player(7, 9)
        self.action_observer.add_player(7, 12)
        self.action_observer.add_player(8, 1)
        self.action_observer.add_player(8, 2)
        self.action_observer.add_player(8, 42)
        player_states = [MockPlayerState(7, 8, [self.action_observer]), MockPlayerState(7, 9, [self.action_observer]),
                         MockPlayerState(7, 12, [self.action_observer]), MockPlayerState(8, 1, [self.action_observer]),
                         MockPlayerState(8, 2, [self.action_observer]), MockPlayerState(8, 42, [self.action_observer])]
        # Create actions to test with
        actions = [IdleAction(3, 8, 7, "reason"), IdleAction(7, 8, 7, "reason"), IdleAction(5, 12, 7, "reason"),
                   IdleAction(4, 2, 8, "reason"), IdleAction(3, 2, 8, "reason"), IdleAction(6, 2, 8, "reason")]
        # Record added actions and update observers with those observers
        actions_by_timepoint: Dict[int, List[Action]] = {}
        actions_by_gen_and_timepoint: Dict[int, Dict[int, List[Action]]] = {}
        actions_by_gen_player_and_timepoint: Dict[int, Dict[int, Dict[int, Action]]] = {}
        for player_state in player_states:
            if player_state.generation not in actions_by_gen_and_timepoint:
                actions_by_gen_and_timepoint[player_state.generation] = {}
            if player_state.generation not in actions_by_gen_player_and_timepoint:
                actions_by_gen_player_and_timepoint[player_state.generation] = {}
            if player_state.player not in actions_by_gen_player_and_timepoint[player_state.generation]:
                actions_by_gen_player_and_timepoint[player_state.generation][player_state.player] = {}
        for action in actions:
            if action.timepoint in actions_by_timepoint:
                actions_by_timepoint[action.timepoint].append(action)
            else:
                actions_by_timepoint[action.timepoint] = [action]
            if action.timepoint in actions_by_gen_and_timepoint[action.generation]:
                actions_by_gen_and_timepoint[action.generation][action.timepoint].append(action)
            else:
                actions_by_gen_and_timepoint[action.generation][action.timepoint] = [action]
            actions_by_gen_player_and_timepoint[action.generation][action.actor][action.timepoint] = action
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # Check relevant data structures have been correctly updated
        self.assertEqual(actions_by_timepoint, self.action_observer.actions,
                         "Should be the same as the actions received from updates")
        self.assertEqual(actions_by_gen_and_timepoint, self.action_observer.actions_by_generation,
                         "Should have formatted correctly into generations")
        self.assertEqual(actions_by_gen_player_and_timepoint,
                         self.action_observer.actions_by_generation_and_player, "Should have formatted correctly into "
                                                                                "generation and players")

    def test_initial_interactions(self):
        # Test interaction data structures have been initialised correctly
        self.assertEqual({}, self.action_observer.interactions)
        self.assertEqual({}, self.action_observer.interactions_by_generation)
        self.assertEqual({}, self.action_observer.interactions_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        # Test interaction data structures have been correctly set up
        self.assertEqual({2: {}, 6: {}}, self.action_observer.interactions_by_generation)
        self.assertEqual({2: {8: {}, 9: {}, 12: {}}, 6: {8: {}, 34: {}}},
                         self.action_observer.interactions_by_generation_and_player)

    def test_add_interactions(self):
        # Create resources (generations, players, player states
        self.action_observer.add_generation(7)
        self.action_observer.add_generation(8)
        self.action_observer.add_player(7, 8)
        self.action_observer.add_player(7, 9)
        self.action_observer.add_player(7, 12)
        self.action_observer.add_player(8, 1)
        self.action_observer.add_player(8, 2)
        self.action_observer.add_player(8, 42)
        player_states = [MockPlayerState(7, 8, [self.action_observer]), MockPlayerState(7, 9, [self.action_observer]),
                         MockPlayerState(7, 12, [self.action_observer]), MockPlayerState(8, 1, [self.action_observer]),
                         MockPlayerState(8, 2, [self.action_observer]), MockPlayerState(8, 42, [self.action_observer])]
        # Create actions to update with
        actions = [InteractionAction(4, 8, 7, "reason", 9, InteractionContent.COOPERATE),
                   InteractionAction(35, 8, 7, "reason", 12, InteractionContent.DEFECT),
                   InteractionAction(6, 9, 7, "reason", 8, InteractionContent.COOPERATE),
                   InteractionAction(10, 9, 7, "reason", 8, InteractionContent.DEFECT),
                   InteractionAction(1, 1, 8, "reason", 2, InteractionContent.COOPERATE),
                   InteractionAction(4, 2, 8, "reason", 1, InteractionContent.COOPERATE),
                   InteractionAction(6, 2, 8, "reason", 1, InteractionContent.COOPERATE),
                   InteractionAction(9, 42, 8, "reason", 2, InteractionContent.DEFECT)]
        # Update observer with actions and record what should have updated within them
        actions_by_timepoint: Dict[int, Action] = {}
        actions_by_gen_and_timepoint: Dict[int, Dict[int, Action]] = {}
        actions_by_gen_player_and_timepoint: Dict[int, Dict[int, Dict[int, Action]]] = {}
        for player_state in player_states:
            if player_state.generation not in actions_by_gen_and_timepoint:
                actions_by_gen_and_timepoint[player_state.generation] = {}
            if player_state.generation not in actions_by_gen_player_and_timepoint:
                actions_by_gen_player_and_timepoint[player_state.generation] = {}
            if player_state.player not in actions_by_gen_player_and_timepoint[player_state.generation]:
                actions_by_gen_player_and_timepoint[player_state.generation][player_state.player] = {}
        for action in actions:
            actions_by_timepoint[action.timepoint] = action
            actions_by_gen_and_timepoint[action.generation][action.timepoint] = action
            actions_by_gen_player_and_timepoint[action.generation][action.actor][action.timepoint] = action
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # check observers have updated correctly
        self.assertEqual(actions_by_timepoint, self.action_observer.interactions,
                         "Should be the same as the actions received from updates")
        self.assertEqual(actions_by_gen_and_timepoint, self.action_observer.interactions_by_generation,
                         "Should have formatted correctly into generations")
        self.assertEqual(actions_by_gen_player_and_timepoint,
                         self.action_observer.interactions_by_generation_and_player,
                         "Should have formatted correctly into generation and players")

    def test_initial_cooperation_rate(self):
        # test cooperation rate data initialisation
        self.assertEqual(None, self.action_observer.cooperation_rate)
        self.assertEqual({}, self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({}, self.action_observer.cooperation_rate_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        # Test cooperation rate data structures have been updated
        self.assertEqual({2: None, 6: None}, self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({2: {8: None, 9: None, 12: None}, 6: {8: None, 34: None}},
                         self.action_observer.cooperation_rate_by_generation_and_player)

    def test_add_actions_coop_defect_rate(self):
        # Set up relevant resources
        self.action_observer.add_generation(7)
        self.action_observer.add_generation(8)
        self.action_observer.add_player(7, 8)
        self.action_observer.add_player(7, 9)
        self.action_observer.add_player(7, 12)
        self.action_observer.add_player(8, 1)
        self.action_observer.add_player(8, 2)
        self.action_observer.add_player(8, 42)
        player_states = [MockPlayerState(7, 8, [self.action_observer]), MockPlayerState(7, 9, [self.action_observer]),
                         MockPlayerState(7, 12, [self.action_observer]), MockPlayerState(8, 1, [self.action_observer]),
                         MockPlayerState(8, 2, [self.action_observer]), MockPlayerState(8, 42, [self.action_observer])]
        actions = [InteractionAction(4, 8, 7, "reason", 9, InteractionContent.COOPERATE),
                   InteractionAction(35, 8, 7, "reason", 12, InteractionContent.DEFECT),
                   InteractionAction(6, 9, 7, "reason", 8, InteractionContent.COOPERATE),
                   InteractionAction(10, 9, 7, "reason", 8, InteractionContent.DEFECT),
                   InteractionAction(1, 1, 8, "reason", 2, InteractionContent.COOPERATE),
                   InteractionAction(4, 2, 8, "reason", 1, InteractionContent.COOPERATE),
                   InteractionAction(6, 2, 8, "reason", 1, InteractionContent.COOPERATE),
                   InteractionAction(9, 42, 8, "reason", 2, InteractionContent.DEFECT)]
        # update observers with actions
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # Check relevant data structures have been updated correctly
        self.assertEqual(int(round(100*(5/8))), self.action_observer.cooperation_rate)
        self.assertEqual({7: 50, 8: 75}, self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({7: {8: 50, 9: 50, 12: None}, 8: {1: 100, 2: 100, 42: 0}},
                         self.action_observer.cooperation_rate_by_generation_and_player)

    def test_initial_social_activeness(self):
        # Test initialisation of social activeness data structures
        self.assertEqual(None, self.action_observer.social_activeness)
        self.assertEqual({}, self.action_observer.social_activeness_by_generation)
        self.assertEqual({}, self.action_observer.social_activeness_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        # Check social activeness data structures have been set up correctly
        self.assertEqual({2: None, 6: None}, self.action_observer.social_activeness_by_generation)
        self.assertEqual({2: {8: None, 9: None, 12: None}, 6: {8: None, 34: None}},
                         self.action_observer.social_activeness_by_generation_and_player)

    def test_add_social_idle_actions(self):
        # Create relevant generations, players and player states
        self.action_observer.add_generation(7)
        self.action_observer.add_generation(8)
        self.action_observer.add_player(7, 8)
        self.action_observer.add_player(7, 9)
        self.action_observer.add_player(7, 12)
        self.action_observer.add_player(8, 1)
        self.action_observer.add_player(8, 2)
        self.action_observer.add_player(8, 42)
        player_states = [MockPlayerState(7, 8, [self.action_observer]),
                         MockPlayerState(7, 9, [self.action_observer]),
                         MockPlayerState(7, 12, [self.action_observer]),
                         MockPlayerState(8, 1, [self.action_observer]),
                         MockPlayerState(8, 2, [self.action_observer]),
                         MockPlayerState(8, 42, [self.action_observer])]
        # Create actions to update with
        actions = [IdleAction(4, 8, 7, "reason"), IdleAction(5, 8, 7, "reason"),
                   GossipAction(6, 8, 7, "reason", 9, 12, GossipContent.POSITIVE),
                   GossipAction(7, 9, 7, "reason", 12, 8, GossipContent.NEGATIVE),
                   IdleAction(1, 1, 8, "reason"), GossipAction(2, 1, 8, "reason", 2, 42, GossipContent.NEGATIVE),
                   IdleAction(3, 42, 8, "reason")]
        # Update observers with actions
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # Check data structures have been updated correctly
        self.assertEqual(int(round(100 * (3 / 7))), self.action_observer.social_activeness)
        self.assertEqual({7: 50, 8: int(round(100*(1/3)))}, self.action_observer.social_activeness_by_generation)
        self.assertEqual({7: {8: int(round(100*(1/3))), 9: 100, 12: None}, 8: {1: 50, 2: None, 42: 0}},
                         self.action_observer.social_activeness_by_generation_and_player)

    def test_initial_positivity_of_gossip(self):
        # test that positivity of gossip data structures have been initialised correctly
        self.assertEqual(None, self.action_observer.positivity_of_gossip_percentage)
        self.assertEqual({}, self.action_observer.positivity_of_gossip_percentage_by_generation)
        self.assertEqual({}, self.action_observer.positivity_of_gossip_percentage_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        # Test data structures have been set up correctly
        self.assertEqual({2: None, 6: None}, self.action_observer.positivity_of_gossip_percentage_by_generation)
        self.assertEqual({2: {8: None, 9: None, 12: None}, 6: {8: None, 34: None}},
                         self.action_observer.positivity_of_gossip_percentage_by_generation_and_player)

    def test_add_social_negative_positive_actions(self):
        # Set up gen, player and player state resources
        self.action_observer.add_generation(7)
        self.action_observer.add_generation(8)
        self.action_observer.add_player(7, 8)
        self.action_observer.add_player(7, 9)
        self.action_observer.add_player(7, 12)
        self.action_observer.add_player(8, 1)
        self.action_observer.add_player(8, 2)
        self.action_observer.add_player(8, 42)
        player_states = [MockPlayerState(7, 8, [self.action_observer]),
                         MockPlayerState(7, 9, [self.action_observer]),
                         MockPlayerState(7, 12, [self.action_observer]),
                         MockPlayerState(8, 1, [self.action_observer]),
                         MockPlayerState(8, 2, [self.action_observer]),
                         MockPlayerState(8, 42, [self.action_observer])]
        # Create appropriated gossip actions
        actions = [GossipAction(4, 8, 7, "reason", 12, 9, GossipContent.POSITIVE),
                   GossipAction(5, 8, 7, "reason", 9, 12, GossipContent.NEGATIVE),
                   GossipAction(6, 8, 7, "reason", 9, 12, GossipContent.POSITIVE),
                   GossipAction(7, 9, 7, "reason", 12, 8, GossipContent.NEGATIVE),
                   GossipAction(1, 1, 8, "reason", 2, 42, GossipContent.POSITIVE),
                   GossipAction(2, 1, 8, "reason", 2, 42, GossipContent.NEGATIVE),
                   GossipAction(3, 42, 8, "reason", 1, 2, GossipContent.POSITIVE)]
        # Update observers with actions
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # Test whether data structures have been updated correctly
        self.assertEqual(int(round(100 * (4 / 7))), self.action_observer.positivity_of_gossip_percentage)
        self.assertEqual({7: 50, 8: 67}, self.action_observer.positivity_of_gossip_percentage_by_generation)
        self.assertEqual({7: {8: 67, 9: 0, 12: None}, 8: {1: 50, 2: None, 42: 100}},
                         self.action_observer.positivity_of_gossip_percentage_by_generation_and_player)

    def test_add_all_actions_at_the_same_time(self):
        # Set up generation, player and player state resources
        self.action_observer.add_generation(7)
        self.action_observer.add_generation(8)
        self.action_observer.add_player(7, 8)
        self.action_observer.add_player(7, 9)
        self.action_observer.add_player(7, 12)
        self.action_observer.add_player(8, 1)
        self.action_observer.add_player(8, 2)
        self.action_observer.add_player(8, 42)
        player_states = [MockPlayerState(7, 8, [self.action_observer]),
                         MockPlayerState(7, 9, [self.action_observer]),
                         MockPlayerState(7, 12, [self.action_observer]),
                         MockPlayerState(8, 1, [self.action_observer]),
                         MockPlayerState(8, 2, [self.action_observer]),
                         MockPlayerState(8, 42, [self.action_observer])]
        # Create appropriate actions
        actions = [GossipAction(4, 8, 7, "reason", 12, 9, GossipContent.POSITIVE),
                   GossipAction(5, 8, 7, "reason", 9, 12, GossipContent.NEGATIVE),
                   GossipAction(6, 8, 7, "reason", 9, 12, GossipContent.POSITIVE),
                   IdleAction(4, 8, 7, "reason"), IdleAction(5, 8, 7, "reason"),
                   GossipAction(6, 8, 7, "reason", 9, 12, GossipContent.POSITIVE),
                   InteractionAction(4, 8, 7, "reason", 9, InteractionContent.COOPERATE),
                   InteractionAction(35, 8, 7, "reason", 12, InteractionContent.DEFECT),
                   GossipAction(7, 9, 7, "reason", 12, 8, GossipContent.NEGATIVE),
                   InteractionAction(6, 9, 7, "reason", 8, InteractionContent.COOPERATE),
                   InteractionAction(10, 9, 7, "reason", 8, InteractionContent.DEFECT),
                   GossipAction(1, 1, 8, "reason", 2, 42, GossipContent.POSITIVE),
                   IdleAction(1, 1, 8, "reason"), GossipAction(2, 1, 8, "reason", 2, 42, GossipContent.NEGATIVE),
                   InteractionAction(1, 1, 8, "reason", 2, InteractionContent.COOPERATE),
                   GossipAction(2, 1, 8, "reason", 2, 42, GossipContent.NEGATIVE),
                   InteractionAction(4, 2, 8, "reason", 1, InteractionContent.COOPERATE),
                   InteractionAction(6, 2, 8, "reason", 1, InteractionContent.COOPERATE),
                   GossipAction(3, 42, 8, "reason", 1, 2, GossipContent.POSITIVE), IdleAction(3, 42, 8, "reason"),
                   InteractionAction(9, 42, 8, "reason", 2, InteractionContent.DEFECT)]
        # Update observer with actions and record what they should have updated in their data structures
        actions_by_timepoint: Dict[int, List[Action]] = {}
        interactions: Dict[int, InteractionAction] = {}
        actions_by_gen_and_timepoint: Dict[int, Dict[int, List[Action]]] = {}
        interactions_by_gen: Dict[int, Dict[int, InteractionAction]] ={}
        actions_by_gen_player_and_timepoint: Dict[int, Dict[int, Dict[int, Action]]] = {}
        interactions_by_gen_and_player: Dict[int, Dict[int, Dict[int, InteractionAction]]] = {}
        for player_state in player_states:
            if player_state.generation not in actions_by_gen_and_timepoint:
                actions_by_gen_and_timepoint[player_state.generation] = {}
                interactions_by_gen[player_state.generation] = {}
            if player_state.generation not in actions_by_gen_player_and_timepoint:
                actions_by_gen_player_and_timepoint[player_state.generation] = {}
                interactions_by_gen_and_player[player_state.generation] = {}
            if player_state.player not in actions_by_gen_player_and_timepoint[player_state.generation]:
                actions_by_gen_player_and_timepoint[player_state.generation][player_state.player] = {}
                interactions_by_gen_and_player[player_state.generation][player_state.player] = {}
        for action in actions:
            if action.type == ActionType.INTERACTION:
                interactions[action.timepoint] = action
                interactions_by_gen[action.generation][action.timepoint] = action
                interactions_by_gen_and_player[action.generation][action.actor][action.timepoint] = action
            if action.timepoint in actions_by_timepoint:
                actions_by_timepoint[action.timepoint].append(action)
            else:
                actions_by_timepoint[action.timepoint] = [action]
            if action.timepoint in actions_by_gen_and_timepoint[action.generation]:
                actions_by_gen_and_timepoint[action.generation][action.timepoint].append(action)
            else:
                actions_by_gen_and_timepoint[action.generation][action.timepoint] = [action]
            actions_by_gen_player_and_timepoint[action.generation][action.actor][action.timepoint] = action
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # Test actions
        self.assertEqual(actions_by_timepoint, self.action_observer.actions, "Should be the same as the actions received "
                                                                "from updates")
        self.assertEqual(actions_by_gen_and_timepoint, self.action_observer.actions_by_generation,
                         "Should have formatted actions correctly into generations")
        self.assertEqual(actions_by_gen_player_and_timepoint,
                         self.action_observer.actions_by_generation_and_player, "Should have formattedactions correctly"
                                                                                " into generation and players")
        # Test interactions
        self.assertEqual(interactions, self.action_observer.interactions)
        self.assertEqual(interactions_by_gen, self.action_observer.interactions_by_generation,
                         "Should have formatted interactions correctly into generations")
        self.assertEqual(interactions_by_gen_and_player,
                         self.action_observer.interactions_by_generation_and_player,
                         "Should have formatted interactions correctly into generations and players")
        # Test cooperation rate
        self.assertEqual(int(round(100 * (5 / 8))), self.action_observer.cooperation_rate, "Community coop rate")
        self.assertEqual({7: 50, 8: 75}, self.action_observer.cooperation_rate_by_generation, "Coop rate by gen")
        self.assertEqual({7: {8: 50, 9: 50, 12: None}, 8: {1: 100, 2: 100, 42: 0}},
                         self.action_observer.cooperation_rate_by_generation_and_player, "Coop rate by gen and player")
        # Test social activeness
        self.assertEqual(int(round(100 * (9 / 13))), self.action_observer.social_activeness, "Community social "
                                                                                             "activeness")
        self.assertEqual({7: int(round(100 * (5 / 7))), 8: int(round(100 * (4 / 6)))},
                         self.action_observer.social_activeness_by_generation, "Social activeness by gen")
        self.assertEqual({7: {8: int(round(100 * (4 / 6))), 9: 100, 12: None}, 8: {1: 75, 2: None, 42: 50}},
                         self.action_observer.social_activeness_by_generation_and_player, "Social activeness by gen "
                                                                                          "and player")
        # Test positivity of gossip
        self.assertEqual(int(round(100 * (5 / 9))), self.action_observer.positivity_of_gossip_percentage, "Positivity"
                                                                                                          " of gossip")
        self.assertEqual({7: int(round(100 * (3 / 5))), 8: 50},
                         self.action_observer.positivity_of_gossip_percentage_by_generation, "Positivity of gossip"
                                                                                             " by gen")
        self.assertEqual({7: {8: 75, 9: 0, 12: None}, 8: {1: 33, 2: None, 42: 100}},
                         self.action_observer.positivity_of_gossip_percentage_by_generation_and_player, "Positivity of"
                                                                                                        "gossip by gen"
                                                                                                        " and player")


class PlayerObserverTests(unittest.TestCase):
    """Test the PlayerObserver class"""

    def setUp(self):
        # Set up relevant resources (community, generation and player ids, observer and player state)
        self.community = random.randint(0, 100)
        self.player_observer = PlayerObserver(self.community)
        self.generation = 0
        self.player = 0
        self.player_state = MockPlayerState(self.generation, self.player, [self.player_observer])

    def test_get_corrupted_observations(self):
        # Observations shouldn't be corrupted to begin with
        self.assertFalse(self.player_observer.corrupted_observations)

    def test_get_community(self):
        # Test that the community has been set and returned correctly
        self.assertEqual(self.community, self.player_observer.community, "Should be the same as the set comumnity")

    def test_get_generation(self):
        # Test that no generations have been added yet
        self.assertEqual([], self.player_observer.generations, "Should be set as empty")

    def test_add_generation(self):
        # Test the adding of generations to the system adds to the relevant data structures
        self.player_observer.add_generation(2)
        self.assertEqual([2], self.player_observer.generations, "Should have added 2")
        self.player_observer.add_generation(19)
        self.assertEqual([2, 19], self.player_observer.generations, "Should have added 19")
        self.player_observer.add_generation(4)
        self.assertEqual([2, 19, 4], self.player_observer.generations, "Should have added 4")
        self.assertFalse(self.player_observer.corrupted_observations)
        self.assertEqual({2: [], 19: [], 4: []}, self.player_observer.players,
                         "Should have added an entry in the players dict for every generation")
        # Should fail to add duplicate generations and record corrupted observations
        with self.assertRaises(RecordingError):
            self.player_observer.add_generation(19)
        self.assertEqual([2, 19, 4], self.player_observer.generations, "Should have failed to add")
        self.assertTrue(self.player_observer.corrupted_observations)

    def test_generation_in_constructor(self):
        # Test adding generation list in constructor initialises relevant data structures
        generations = [23, 5, 78, 29, 9]
        new_action_observer = ActionObserver(self.community, generations)
        self.assertEqual(generations, new_action_observer.generations, "Should have the same generations as set in"
                                                                       " constructor")
        self.assertEqual({23: [], 5: [], 78: [], 29: [], 9: []}, new_action_observer.players,
                         "Should have added an entry in the players dict for every generation")

    def test_generations_in_constructor_with_identical(self):
        # Adding duplicate generations in the constructor should record corrupted observations
        generations = [23, 5, 78, 23, 9, 5]
        with self.assertRaises(RecordingError):
            new_action_observer = ActionObserver(self.community, generations)
            self.assertEqual(self.community, new_action_observer.community, "Should be the same as the set comumnity")
            self.assertTrue(new_action_observer.corrupted_observations)

    def test_get_players(self):
        # Check that no players have been added at the start
        self.assertEqual({}, self.player_observer.players, "Should be empty with no generations")

    def test_add_players(self):
        # add generations
        self.player_observer.add_generation(2)
        self.player_observer.add_generation(6)
        self.player_observer.add_generation(78)
        # add players
        self.player_observer.add_player(2, 8)
        self.player_observer.add_player(2, 9)
        self.player_observer.add_player(2, 12)
        self.player_observer.add_player(6, 8)
        self.player_observer.add_player(6, 34)
        # Check relevant data structures have been updated
        self.assertEqual({2: [8, 9, 12], 6: [8, 34], 78: []}, self.player_observer.players, "Should have added the"
                                                                                            " correct gens and players")

    def test_add_players_to_non_existent_gen(self):
        # Check corrupted observation is caught
        with self.assertRaises(RecordingError):
            self.player_observer.add_player(6, 34)
        self.assertTrue(self.player_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_add_identical_players(self):
        # Check corrupted observation is caught
        self.player_observer.add_generation(2)
        self.player_observer.add_player(2, 8)
        with self.assertRaises(RecordingError):
            self.player_observer.add_player(2, 8)
        self.assertTrue(self.player_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_initial_fitness(self):
        # Create relevant mock player states
        player_states = [MockPlayerState(1, 2), MockPlayerState(1, 3), MockPlayerState(1, 5),
                         MockPlayerState(2, 5), MockPlayerState(2, 7), MockPlayerState(2, 1)]
        # Add generations and players to observer
        for player in player_states:
            if player.generation not in self.player_observer.generations:
                self.player_observer.add_generation(player.generation)
            self.player_observer.add_player(player.generation, player.player)
        # Check relevant data structures have been initialised correctly
        self.assertEqual(0, self.player_observer.community_fitness)
        self.assertEqual({1: 0, 2: 0}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {2: 0, 3: 0, 5: 0}, 2: {5: 0, 7: 0, 1: 0}},
                         self.player_observer.fitness_by_generation_and_player)

    def test_update(self):
        # Create relevant resources
        self.player_observer.add_generation(self.generation)
        self.player_observer.add_player(self.generation, self.player)
        # Update fitness and notify observer of this update
        self.player_state.fitness_update = 3
        self.player_observer.update(self.player_state)
        # Check relevant data structures have been updated correcly
        self.assertEqual(3, self.player_observer.community_fitness, "Should have updated the fitness by 3")
        self.assertEqual({self.generation: 3}, self.player_observer.fitness_by_generation)
        self.assertEqual({self.generation: {self.player: 3}}, self.player_observer.fitness_by_generation_and_player)

    def test_update_non_existent_gen(self):
        # Check corrupted observation is caught
        self.player_state.fitness_update = 3
        with self.assertRaises(RecordingError):
            self.player_observer.update(self.player_state)

    def test_update_non_existent_player(self):
        # Check corrupted observation is caught
        self.player_state.fitness_update = 3
        self.player_observer.add_generation(self.generation)
        with self.assertRaises(RecordingError):
            self.player_observer.update(self.player_state)

    def test_multiple_updates(self):
        # Create relevant player states
        player_states = [MockPlayerState(1, 2), MockPlayerState(1, 3), MockPlayerState(1, 5),
                         MockPlayerState(2, 5), MockPlayerState(2, 7), MockPlayerState(2, 1)]
        # Add relevant players and generations to the observer
        for player in player_states:
            if player.generation not in self.player_observer.generations:
                self.player_observer.add_generation(player.generation)
            self.player_observer.add_player(player.generation, player.player)
        # Create appropriate fitness updates
        fitness_updates = [{'player_state': player_states[0], 'update': -2},
                           {'player_state': player_states[2], 'update': 4},
                           {'player_state': player_states[2], 'update': -1},
                           {'player_state': player_states[3], 'update': 7},
                           {'player_state': player_states[3], 'update': -2},
                           {'player_state': player_states[4], 'update': 9}]
        # Update the fitness and notify the observer
        for update in fitness_updates:
            update['player_state'].fitness_update = update['update']
            self.player_observer.update(update['player_state'])
        # Check relevant data structures have been updated
        self.assertEqual(15, self.player_observer.community_fitness)
        self.assertEqual({1: 1, 2: 14}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {2: -2, 3: 0, 5: 3}, 2: {5: 5, 7: 9, 1: 0}},
                         self.player_observer.fitness_by_generation_and_player)


class PlayerActionObserverIntegrationTests(unittest.TestCase):
    """Test the integration of th ActionObserver and Player classes"""

    def setUp(self):
        # Create the app
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        # Create the community, generation, player, observer and add the generation and player to the observer
        self.community = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        self.generation = 1
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", Config.AGENTS_URL + 'generation', json=generation_payload)
        self.action_observer = ActionObserver(self.community)
        self.player = Player(1, Strategy('Random', 'Random', 'Void', []), self.community, 1, [self.action_observer])
        self.action_observer.add_generation(1)
        self.action_observer.add_player(1, 1)

    def test_get_action(self):
        # Get a action from the player
        action = self.player.decide(3)
        # Test that relevant data structures have had the action added
        self.assertEqual({3: [action]}, self.action_observer.actions)
        self.assertEqual({1: {3: [action]}}, self.action_observer.actions_by_generation)
        self.assertEqual({1: {1: {3: action}}}, self.action_observer.actions_by_generation_and_player)
        # Check that the relevant data structures have had their data updated
        if action.type is ActionType.INTERACTION:
            interaction: InteractionAction = action
            if interaction.action == InteractionContent.COOPERATE:
                self.assertEqual(100, self.action_observer.cooperation_rate)
                self.assertEqual({1: 100}, self.action_observer.actions_by_generation)
                self.assertEqual({1: {1: 100}}, self.action_observer.actions_by_generation_and_player)
            else:
                self.assertEqual(0, self.action_observer.cooperation_rate)
                self.assertEqual({1: 0}, self.action_observer.actions_by_generation)
                self.assertEqual({1: {1: 0}}, self.action_observer.actions_by_generation_and_player)
        elif action.type == ActionType.IDLE:
            self.assertEqual(0, self.action_observer.social_activeness)
            self.assertEqual({1: 0}, self.action_observer.social_activeness_by_generation)
            self.assertEqual({1: {1: 0}}, self.action_observer.social_activeness_by_generation_and_player)
        else:
            gossip_action: GossipAction = action
            self.assertEqual(100, self.action_observer.social_activeness)
            self.assertEqual({1: 100}, self.action_observer.social_activeness_by_generation)
            self.assertEqual({1: {1: 100}}, self.action_observer.social_activeness_by_generation_and_player)
            if gossip_action.gossip == GossipContent.POSITIVE:
                self.assertEqual(100, self.action_observer.positivity_of_gossip_percentage)
                self.assertEqual({1: 100}, self.action_observer.social_activeness_by_generation)
                self.assertEqual({1: {1: 100}}, self.action_observer.social_activeness_by_generation_and_player)
            else:
                self.assertEqual(0, self.action_observer.positivity_of_gossip_percentage)
                self.assertEqual({1: 0}, self.action_observer.social_activeness_by_generation)
                self.assertEqual({1: {1: 0}}, self.action_observer.social_activeness_by_generation_and_player)


class PlayerPlayerObserverIntegrationTests(unittest.TestCase):
    """Test the integration of the Player and PlayerObserver classes"""

    def setUp(self):
        # Create app
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        # Create resources (community, generation, player and state, and add to observer)
        self.community = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        self.generation = 1
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", Config.AGENTS_URL + 'generation', json=generation_payload)
        self.player_observer = PlayerObserver(self.community)
        self.player = Player(1, Strategy('Random', 'Random', 'Void', []), self.community, 1, [self.player_observer])
        self.player_observer.add_generation(1)
        self.player_observer.add_player(1, 1)

    def tearDown(self):
        self.app_context.pop()

    def test_update_fitness(self):
        # Update the players fitness
        self.player.update_fitness(5)
        # Check relevant data has been updated
        self.assertEqual(5, self.player_observer.community_fitness)
        self.assertEqual({1: 5}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 5}}, self.player_observer.fitness_by_generation_and_player)
        # Update the players fitness
        self.player.update_fitness(-2)
        # Check relevant data has been updated
        self.assertEqual(3, self.player_observer.community_fitness)
        self.assertEqual({1: 3}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 3}}, self.player_observer.fitness_by_generation_and_player)
        # Update the players fitness
        self.player.update_fitness(50)
        # Check relevant data has been updated
        self.assertEqual(53, self.player_observer.community_fitness)
        self.assertEqual({1: 53}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 53}}, self.player_observer.fitness_by_generation_and_player)
        # Update the players fitness
        self.player.update_fitness(-70)
        # Check relevant data has been updated (cannot go below zero)
        self.assertEqual(0, self.player_observer.community_fitness)
        self.assertEqual({1: 0}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 0}}, self.player_observer.fitness_by_generation_and_player)

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
from flask import current_app


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

    def test_initial_actions(self):
        self.assertEqual([], self.action_observer.actions)
        self.assertEqual({}, self.action_observer.actions_by_generation)
        self.assertEqual({}, self.action_observer.actions_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        self.assertEqual({2: [], 6: []}, self.action_observer.actions_by_generation)
        self.assertEqual({2: {8: [], 9: [], 12: []}, 6: {8: [], 34: []}},
                         self.action_observer.actions_by_generation_and_player)

    def test_add_actions(self):
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
        actions = [IdleAction(3, 8, 7), IdleAction(7, 8, 7), IdleAction(5, 12, 7), IdleAction(4, 2, 8),
                   IdleAction(3, 2, 8), IdleAction(6, 2, 8)]
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        self.assertEqual(actions, self.action_observer.actions, "Should be the same as the actions received "
                                                                "from updates")
        self.assertEqual({7: actions[0:3], 8: actions[3:6]}, self.action_observer.actions_by_generation,
                         "Should have formatted correctly into generations")
        self.assertEqual({7: {8: actions[0:2], 9: [], 12: [actions[2]]},
                          8: {1: [], 2: actions[3:6], 42: []}},
                         self.action_observer.actions_by_generation_and_player, "Should have formatted correctly into "
                                                                                "generation and players")

    def test_initial_interactions(self):
        self.assertEqual([], self.action_observer.interactions)
        self.assertEqual({}, self.action_observer.interactions_by_generation)
        self.assertEqual({}, self.action_observer.interactions_by_generation_and_player)
        self.action_observer.add_generation(2)
        self.action_observer.add_generation(6)
        self.action_observer.add_player(2, 8)
        self.action_observer.add_player(2, 9)
        self.action_observer.add_player(2, 12)
        self.action_observer.add_player(6, 8)
        self.action_observer.add_player(6, 34)
        self.assertEqual({2: [], 6: []}, self.action_observer.interactions_by_generation)
        self.assertEqual({2: {8: [], 9: [], 12: []}, 6: {8: [], 34: []}},
                         self.action_observer.interactions_by_generation_and_player)

    def test_add_interactions(self):
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
        actions = [InteractionAction(4, 8, 7, 9, InteractionContent.COOPERATE),
                   InteractionAction(35, 8, 7, 12, InteractionContent.DEFECT),
                   InteractionAction(6, 9, 7, 8, InteractionContent.COOPERATE),
                   InteractionAction(10, 9, 7, 8, InteractionContent.DEFECT),
                   InteractionAction(1, 1, 8, 2, InteractionContent.COOPERATE),
                   InteractionAction(4, 2, 8, 1, InteractionContent.COOPERATE),
                   InteractionAction(6, 2, 8, 1, InteractionContent.COOPERATE),
                   InteractionAction(9, 42, 8, 2, InteractionContent.DEFECT)]
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        self.assertEqual(actions, self.action_observer.interactions)
        self.assertEqual({7: actions[0:4], 8: actions[4:8]}, self.action_observer.interactions_by_generation,
                         "Should have formatted correctly into generations")
        self.assertEqual({7: {8: actions[0:2], 9: actions[2:4], 12: []},
                          8: {1: [actions[4]], 2: actions[5:7], 42: [actions[7]]}},
                         self.action_observer.interactions_by_generation_and_player,
                         "Should have formatted correctly into generations and players")

    def test_initial_cooperation_rate(self):
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
        self.assertEqual({2: None, 6: None}, self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({2: {8: None, 9: None, 12: None}, 6: {8: None, 34: None}},
                         self.action_observer.cooperation_rate_by_generation_and_player)

    def test_add_actions_coop_defect_rate(self):
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
        actions = [InteractionAction(4, 8, 7, 9, InteractionContent.COOPERATE),
                   InteractionAction(35, 8, 7, 12, InteractionContent.DEFECT),
                   InteractionAction(6, 9, 7, 8, InteractionContent.COOPERATE),
                   InteractionAction(10, 9, 7, 8, InteractionContent.DEFECT),
                   InteractionAction(1, 1, 8, 2, InteractionContent.COOPERATE),
                   InteractionAction(4, 2, 8, 1, InteractionContent.COOPERATE),
                   InteractionAction(6, 2, 8, 1, InteractionContent.COOPERATE),
                   InteractionAction(9, 42, 8, 2, InteractionContent.DEFECT)]
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        self.assertEqual(int(round(100*(5/8))), self.action_observer.cooperation_rate)
        self.assertEqual({7: 50, 8: 75}, self.action_observer.cooperation_rate_by_generation)
        self.assertEqual({7: {8: 50, 9: 50, 12: None}, 8: {1: 100, 2: 100, 42: 0}},
                         self.action_observer.cooperation_rate_by_generation_and_player)

    def test_initial_social_activeness(self):
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
        self.assertEqual({2: None, 6: None}, self.action_observer.social_activeness_by_generation)
        self.assertEqual({2: {8: None, 9: None, 12: None}, 6: {8: None, 34: None}},
                         self.action_observer.social_activeness_by_generation_and_player)

    def test_add_social_idle_actions(self):
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
        actions = [IdleAction(4, 8, 7), IdleAction(5, 8, 7), GossipAction(6, 8, 7, 9, 12, GossipContent.POSITIVE),
                   GossipAction(7, 9, 7, 12, 8, GossipContent.NEGATIVE),
                   IdleAction(1, 1, 8), GossipAction(2, 1, 8, 2, 42, GossipContent.NEGATIVE),
                   IdleAction(3, 42, 8)]
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        self.assertEqual(int(round(100 * (3 / 7))), self.action_observer.social_activeness)
        self.assertEqual({7: 50, 8: int(round(100*(1/3)))}, self.action_observer.social_activeness_by_generation)
        self.assertEqual({7: {8: int(round(100*(1/3))), 9: 100, 12: None}, 8: {1: 50, 2: None, 42: 0}},
                         self.action_observer.social_activeness_by_generation_and_player)

    def test_initial_positivity_of_gossip(self):
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
        self.assertEqual({2: None, 6: None}, self.action_observer.positivity_of_gossip_percentage_by_generation)
        self.assertEqual({2: {8: None, 9: None, 12: None}, 6: {8: None, 34: None}},
                         self.action_observer.positivity_of_gossip_percentage_by_generation_and_player)

    def test_add_social_negative_positive_actions(self):
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
        actions = [GossipAction(4, 8, 7, 12, 9, GossipContent.POSITIVE),
                   GossipAction(5, 8, 7, 9, 12, GossipContent.NEGATIVE),
                   GossipAction(6, 8, 7, 9, 12, GossipContent.POSITIVE),
                   GossipAction(7, 9, 7, 12, 8, GossipContent.NEGATIVE),
                   GossipAction(1, 1, 8, 2, 42, GossipContent.POSITIVE),
                   GossipAction(2, 1, 8, 2, 42, GossipContent.NEGATIVE),
                   GossipAction(3, 42, 8, 1, 2, GossipContent.POSITIVE)]
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        self.assertEqual(int(round(100 * (4 / 7))), self.action_observer.positivity_of_gossip_percentage)
        self.assertEqual({7: 50, 8: 67}, self.action_observer.positivity_of_gossip_percentage_by_generation)
        self.assertEqual({7: {8: 67, 9: 0, 12: None}, 8: {1: 50, 2: None, 42: 100}},
                         self.action_observer.positivity_of_gossip_percentage_by_generation_and_player)

    def test_add_all_actions_at_the_same_time(self):
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
        actions = [GossipAction(4, 8, 7, 12, 9, GossipContent.POSITIVE),
                   GossipAction(5, 8, 7, 9, 12, GossipContent.NEGATIVE),
                   GossipAction(6, 8, 7, 9, 12, GossipContent.POSITIVE),
                   IdleAction(4, 8, 7), IdleAction(5, 8, 7), GossipAction(6, 8, 7, 9, 12, GossipContent.POSITIVE),
                   InteractionAction(4, 8, 7, 9, InteractionContent.COOPERATE),
                   InteractionAction(35, 8, 7, 12, InteractionContent.DEFECT),
                   GossipAction(7, 9, 7, 12, 8, GossipContent.NEGATIVE),
                   InteractionAction(6, 9, 7, 8, InteractionContent.COOPERATE),
                   InteractionAction(10, 9, 7, 8, InteractionContent.DEFECT),
                   GossipAction(1, 1, 8, 2, 42, GossipContent.POSITIVE),
                   IdleAction(1, 1, 8), GossipAction(2, 1, 8, 2, 42, GossipContent.NEGATIVE),
                   InteractionAction(1, 1, 8, 2, InteractionContent.COOPERATE),
                   GossipAction(2, 1, 8, 2, 42, GossipContent.NEGATIVE),
                   InteractionAction(4, 2, 8, 1, InteractionContent.COOPERATE),
                   InteractionAction(6, 2, 8, 1, InteractionContent.COOPERATE),
                   GossipAction(3, 42, 8, 1, 2, GossipContent.POSITIVE), IdleAction(3, 42, 8),
                   InteractionAction(9, 42, 8, 2, InteractionContent.DEFECT)]
        interactions = [action for action in actions if type(action) is InteractionAction]
        for action in actions:
            for player_state in player_states:
                if player_state.player == action.actor and action.generation == player_state.generation:
                    player_state.new_action = action
                    self.action_observer.update(player_state)
        # Test actions
        self.assertEqual(actions, self.action_observer.actions, "Should be the same as the actions received "
                                                                "from updates")
        self.assertEqual({7: actions[0:11], 8: actions[11:21]}, self.action_observer.actions_by_generation,
                         "Should have formatted actions correctly into generations")
        self.assertEqual({7: {8: actions[0:8], 9: actions[8:11], 12: []},
                          8: {1: actions[11:16], 2: actions[16:18], 42: actions[18:21]}},
                         self.action_observer.actions_by_generation_and_player, "Should have formattedactions correctly"
                                                                                " into generation and players")
        # Test interactions
        self.assertEqual(interactions, self.action_observer.interactions)
        self.assertEqual({7: interactions[0:4], 8: interactions[4:8]}, self.action_observer.interactions_by_generation,
                         "Should have formatted interactions correctly into generations")
        self.assertEqual({7: {8: interactions[0:2], 9: interactions[2:4], 12: []},
                          8: {1: [interactions[4]], 2: interactions[5:7], 42: [interactions[7]]}},
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

    def setUp(self):
        self.community = random.randint(0, 100)
        self.player_observer = PlayerObserver(self.community)
        self.generation = 0
        self.player = 0
        self.player_state = MockPlayerState(self.generation, self.player, [self.player_observer])

    def test_get_corrupted_observations(self):
        self.assertFalse(self.player_observer.corrupted_observations)

    def test_get_community(self):
        self.assertEqual(self.community, self.player_observer.community, "Should be the same as the set comumnity")

    def test_get_generation(self):
        self.assertEqual([], self.player_observer.generations, "Should be set as empty")

    def test_add_generation(self):
        self.player_observer.add_generation(2)
        self.assertEqual([2], self.player_observer.generations, "Should have added 2")
        self.player_observer.add_generation(19)
        self.assertEqual([2, 19], self.player_observer.generations, "Should have added 19")
        self.player_observer.add_generation(4)
        self.assertEqual([2, 19, 4], self.player_observer.generations, "Should have added 4")
        self.assertFalse(self.player_observer.corrupted_observations)
        self.assertEqual({2: [], 19: [], 4: []}, self.player_observer.players,
                         "Should have added an entry in the players dict for every generation")
        with self.assertRaises(RecordingError):
            self.player_observer.add_generation(19)
        self.assertEqual([2, 19, 4], self.player_observer.generations, "Should have failed to add")
        self.assertTrue(self.player_observer.corrupted_observations)

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
        self.assertEqual({2: [8, 9, 12], 6: [8, 34], 78: []}, self.player_observer.players, "Should have added the"
                                                                                            " correct gens and players")

    def test_add_players_to_non_existent_gen(self):
        with self.assertRaises(RecordingError):
            self.player_observer.add_player(6, 34)
        self.assertTrue(self.player_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_add_identical_players(self):
        self.player_observer.add_generation(2)
        self.player_observer.add_player(2, 8)
        with self.assertRaises(RecordingError):
            self.player_observer.add_player(2, 8)
        self.assertTrue(self.player_observer.corrupted_observations, "Should have recorded possible corrupted "
                                                                     "observations")

    def test_initial_fitness(self):
        player_states = [MockPlayerState(1, 2), MockPlayerState(1, 3), MockPlayerState(1, 5),
                         MockPlayerState(2, 5), MockPlayerState(2, 7), MockPlayerState(2, 1)]
        for player in player_states:
            if player.generation not in self.player_observer.generations:
                self.player_observer.add_generation(player.generation)
            self.player_observer.add_player(player.generation, player.player)
        self.assertEqual(0, self.player_observer.community_fitness)
        self.assertEqual({1: 0, 2: 0}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {2: 0, 3: 0, 5: 0}, 2: {5: 0, 7: 0, 1: 0}},
                         self.player_observer.fitness_by_generation_and_player)

    def test_update(self):
        self.player_observer.add_generation(self.generation)
        self.player_observer.add_player(self.generation, self.player)
        self.player_state.fitness_update = 3
        self.player_observer.update(self.player_state)
        self.assertEqual(3, self.player_observer.community_fitness, "Should have updated the fitness by 3")
        self.assertEqual({self.generation: 3}, self.player_observer.fitness_by_generation)
        self.assertEqual({self.generation: {self.player: 3}}, self.player_observer.fitness_by_generation_and_player)

    def test_update_non_existent_gen(self):
        self.player_state.fitness_update = 3
        with self.assertRaises(RecordingError):
            self.player_observer.update(self.player_state)

    def test_update_non_existent_player(self):
        self.player_state.fitness_update = 3
        self.player_observer.add_generation(self.generation)
        with self.assertRaises(RecordingError):
            self.player_observer.update(self.player_state)

    def test_multiple_updates(self):
        player_states = [MockPlayerState(1, 2), MockPlayerState(1, 3), MockPlayerState(1, 5),
                         MockPlayerState(2, 5), MockPlayerState(2, 7), MockPlayerState(2, 1)]
        for player in player_states:
            if player.generation not in self.player_observer.generations:
                self.player_observer.add_generation(player.generation)
            self.player_observer.add_player(player.generation, player.player)
        fitness_updates = [{'player_state': player_states[0], 'update': -2},
                           {'player_state': player_states[2], 'update': 4},
                           {'player_state': player_states[2], 'update': -1},
                           {'player_state': player_states[3], 'update': 7},
                           {'player_state': player_states[3], 'update': -2},
                           {'player_state': player_states[4], 'update': 9}]
        for update in fitness_updates:
            update['player_state'].fitness_update = update['update']
            self.player_observer.update(update['player_state'])
        self.assertEqual(15, self.player_observer.community_fitness)
        self.assertEqual({1: 1, 2: 14}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {2: -2, 3: 0, 5: 3}, 2: {5: 5, 7: 9, 1: 0}},
                         self.player_observer.fitness_by_generation_and_player)


class PlayerActionObserverIntegrationTests(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", current_app.config['AGENTS_URL'] + 'community').json()['id']
        self.generation = 1
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", current_app.config['AGENTS_URL'] + 'generation', json=generation_payload)
        self.action_observer = ActionObserver(self.community)
        self.player = Player(1, {'name': 'Random', 'options': []}, self.community, 1, [self.action_observer])
        self.action_observer.add_generation(1)
        self.action_observer.add_player(1, 1)

    def test_get_action(self):
        action = self.player.decide(3)
        self.assertEqual([action], self.action_observer.actions)
        self.assertEqual({1: [action]}, self.action_observer.actions_by_generation)
        self.assertEqual({1: {1: [action]}}, self.action_observer.actions_by_generation_and_player)
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

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", current_app.config['AGENTS_URL'] + 'community').json()['id']
        self.generation = 1
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", current_app.config['AGENTS_URL'] + 'generation', json=generation_payload)
        self.player_observer = PlayerObserver(self.community)
        self.player = Player(1, {'name': 'Random', 'options': []}, self.community, 1, [self.player_observer])
        self.player_observer.add_generation(1)
        self.player_observer.add_player(1, 1)

    def tearDown(self):
        self.app_context.pop()

    def test_update_fitness(self):
        self.player.update_fitness(5)
        self.assertEqual(5, self.player_observer.community_fitness)
        self.assertEqual({1: 5}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 5}}, self.player_observer.fitness_by_generation_and_player)
        self.player.update_fitness(-2)
        self.assertEqual(3, self.player_observer.community_fitness)
        self.assertEqual({1: 3}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 3}}, self.player_observer.fitness_by_generation_and_player)
        self.player.update_fitness(50)
        self.assertEqual(53, self.player_observer.community_fitness)
        self.assertEqual({1: 53}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 53}}, self.player_observer.fitness_by_generation_and_player)
        self.player.update_fitness(-70)
        self.assertEqual(0, self.player_observer.community_fitness)
        self.assertEqual({1: 0}, self.player_observer.fitness_by_generation)
        self.assertEqual({1: {1: 0}}, self.player_observer.fitness_by_generation_and_player)

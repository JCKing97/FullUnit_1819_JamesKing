"""player_tests.py: Test the functionality of the player_logic.py module"""

from .player_logic import Player, DecisionException, PlayerCreationException, PlayerState
from .observation_logic import Observer
from .action_logic import IdleAction, Action, InteractionAction, InteractionContent
from typing import List
from app import create_app
from tests.test_config import TestConfig
import unittest
import requests
from .indir_rec_config import Config
from .strategy_logic import Strategy


class PlayerTest(unittest.TestCase):
    """Test the Player class"""

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        self.generation = 0
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", Config.AGENTS_URL + 'generation', json=generation_payload)

    def tearDown(self):
        self.app_context.pop()

    def test_create_player(self):
        """Test creating a player"""
        try:
            Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")

    def test_incorrect_strategy_create_player(self):
        """Test creating a player with a strategy not in the system"""
        with self.assertRaises(PlayerCreationException):
            Player(0, Strategy("Incorrect", "Void", "Void", []), self.community, self.generation)

    def test_multiple_players_same_id(self):
        Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        with self.assertRaises(PlayerCreationException):
            Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)

    def test_player_get_id(self):
        """Test the id method of the Player class"""
        try:
            player = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        self.assertEqual(0, player.id)

    def test_get_strategy(self):
        strategy = Strategy("Defector", "Lazy", "Void", [])
        player = Player(0, strategy, self.community, self.generation)
        self.assertEqual(strategy, player.strategy)

    def test_player_get_fitness_start(self):
        """Test the get_fitness method of the player at the start of it's life"""
        try:
            player = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        self.assertEqual(0, player.fitness)

    def test_player_update_get_fitness(self):
        """Test the update_fitness() and get_fitness methods of a player together"""
        try:
            player = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        player.update_fitness(-4)
        self.assertEqual(0, player.fitness)
        player.update_fitness(7)
        self.assertEqual(7, player.fitness)

    def test_player_decide(self):
        """Test the decision making of the player"""
        try:
            player = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        try:
            self.assertEqual(IdleAction, type(player.decide(2)))
        except DecisionException:
            self.fail("Shouldn't have failed to make a decision")

    def test_send_donor_percept_decide(self):
        """Test sending a percept to a player that they are a donor, and getting a decision to defect back"""
        try:
            donor = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
            recipient = Player(1, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        except PlayerCreationException:
            self.fail("Shouldn't have failed to create player")
        interaction_payload = {'donor': donor.id, 'recipient': recipient.id, 'timepoint': 2,
                               'community': self.community, 'generation': self.generation}
        interaction_response = requests.request("POST", Config.AGENTS_URL + 'percept/interaction',
                                                json=interaction_payload)
        self.assertEqual(interaction_response.status_code, 200)
        self.assertTrue(interaction_response.json()['success'], msg="Should have been successful sending this percept")
        try:
            action = donor.decide(2)
            self.assertEqual(InteractionAction, type(action), msg="Agent is in an interaction so should act"
                                                                  " accordingly")
            interaction: InteractionAction = action
            self.assertEqual(InteractionContent.DEFECT, interaction.action, msg="Should have defected")
        except DecisionException:
            self.fail("Shouldn't have failed to make a decision")

    def test_set_percept(self):
        perceiver = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        gossiper = Player(1, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        about = Player(2, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        percept = {'community': self.community, 'generation': self.generation, 'perceiver': perceiver.id,
                   'gossip': 'negative', 'about': about.id, 'gossiper': gossiper.id, 'timepoint': 3}
        perceiver.set_perception(percept)
        self.assertEqual(perceiver._percepts[3], [percept])

    def test_perceive(self):
        perceiver = Player(0, Strategy("Standing Discriminator", "Lazy", "Trusting", []), self.community,
                           self.generation)
        gossiper = Player(1, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        about = Player(2, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        percept = {'community': self.community, 'generation': self.generation, 'perceiver': perceiver.id,
                   'gossip': 'negative', 'about': about.id, 'gossiper': gossiper.id, 'timepoint': 3,
                   'type': 'action/gossip'}
        perceiver.set_perception(percept)
        self.assertEqual(perceiver._percepts[3], [percept])
        perceiver.perceive(4)
        belief_payload = {'timepoint': 4, 'community': self.community, 'generation': self.generation,
                          'perceiver': perceiver.id, 'about': about.id}
        belief_response = requests.request("GET", Config.AGENTS_URL + 'belief/standing',
                                           params=belief_payload).json()
        self.assertEqual(belief_response['standing'], "bad", "Should have sent the percept changing the belief"
                                                             " of the player")


class TestObserver(Observer):

    def __init__(self, community: int, generations: List[int]):
        self._community = community
        self._generations = generations
        self._players = []
        self._corrupted_observations = False
        self._latest_obs = {'action': None, 'fitness': 0}

    def update(self, player_state: PlayerState) -> None:
        self._latest_obs = {'action': player_state.new_action, 'fitness': player_state.fitness_update}

    @property
    def latest_observations(self):
        return self._latest_obs

    @property
    def community(self) -> int:
        return self._community

    @property
    def generations(self) -> List[int]:
        return self._generations

    def add_generation(self, generation: int) -> None:
        self._generations.append(generation)

    def add_player(self, generation: int, player: int) -> None:
        self._players.append(player)

    @property
    def corrupted_observations(self) -> bool:
        return self._corrupted_observations


class PlayerStateTests(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        self.generation = 0
        self.player = 0
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", Config.AGENTS_URL + 'generation', json=generation_payload)
        self.player_state = PlayerState(self.generation, self.player)
        self.observer = TestObserver(self.community, [self.generation])
        self.player_state.attach(self.observer)

    def tearDown(self):
        self.app_context.pop()

    def test_get_gen(self):
        self.assertEqual(self.generation, self.player_state.generation, "should be the same as set")

    def test_get_player(self):
        self.assertEqual(self.player, self.player_state.player, "Should be the same as set")

    def test_get_new_action(self):
        self.assertEqual(None, self.player_state.new_action, "Should be None to start with")

    def test_set_then_get_new_action_notify(self):
        action = IdleAction(3, self.player, self.generation, "reason")
        self.player_state.new_action = action
        self.assertEqual(None, self.player_state.new_action, "Should be none as the state is clear after the notification to"
                                                   "observers")
        self.assertEqual(self.observer.latest_observations['action'], action, "Should have notified the observer"
                                                                             "of the latest action")

    def test_get_fitness_update(self):
        self.assertEqual(None, self.player_state.new_action, "Should be None to start with")

    def test_set_then_fitness_update(self):
        self.player_state.fitness_update = 2
        self.assertEqual(0, self.player_state.fitness_update, "Should be reset to zero as the observers would be"
                                                              " notified in the setting of it")
        self.assertEqual(2, self.observer.latest_observations['fitness'], "Should have notified the observer"
                                                                               " of the fitness update")

    def test_detach(self):
        action = IdleAction(3, self.player, self.generation, "reason")
        self.player_state.detach(self.observer)
        self.player_state.fitness_update = 2
        self.assertEqual(0, self.player_state.fitness_update, "Should be reset to zero as the observers would be"
                                                              " notified in the setting of it")
        self.assertEqual(0, self.observer.latest_observations['fitness'], "Should not notify the observer"
                                                                          "as it is detached")
        self.player_state.new_action = action
        self.assertEqual(None, self.player_state.new_action,
                         "Should be none as the state is clear after the notification to"
                         "observers")
        self.assertEqual(None, self.observer.latest_observations['action'], "Should not notify the observer "
                                                                            "as it is detached")

    def test_detach_doesnt_exist(self):
        new_observer = TestObserver(self.community, [self.generation])
        with self.assertRaises(ValueError):
            self.player_state.detach(new_observer)

    def test_observers_constructor(self):
        new_observer = TestObserver(self.community, [self.generation])
        new_player_state = PlayerState(self.generation, self.player, [new_observer])
        action = IdleAction(3, self.player, self.generation, "reason")
        new_player_state.new_action = action
        self.assertEqual(action, new_observer.latest_observations['action'], "Should notify attached observer from"
                                                                             " constructor")

    def test_no_observers_constructor(self):
        new_player_state = PlayerState(self.generation, self.player, [])
        self.assertEqual([], new_player_state._observers, "Should be empty")


class PlayerAndStateIntegrationTests(unittest.TestCase):

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()
        self.community = requests.request("POST", Config.AGENTS_URL + 'community').json()['id']
        self.generation = 0
        self.player = 0
        generation_payload = {"community": self.community, "generation": self.generation}
        requests.request("POST", Config.AGENTS_URL + 'generation', json=generation_payload)
        self.player = Player(0, Strategy("Defector", "Lazy", "Void", []), self.community, self.generation)
        self.observer = TestObserver(self.community, [self.generation])
        self.player.player_state.attach(self.observer)

    def tearDown(self):
        self.app_context.pop()

    def test_update_fitness(self):
        self.player.update_fitness(3)
        self.assertEqual(3, self.observer.latest_observations['fitness'], "Should have been notified of the "
                                                                          "fitness update")

    def test_new_action(self):
        action: Action = self.player.decide(3)
        self.assertEqual(action, self.observer.latest_observations['action'], "Should have been notified of the "
                                                                              "players decision")





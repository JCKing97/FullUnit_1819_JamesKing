"""results_logic.py: Contains the logic to record the results and statistics on a community simulation"""

__author__ = "James King"

from .action_logic import Action, ActionType, InteractionContent, GossipContent
from typing import List, Dict
from .community_logic import Community
from .player_logic import PlayerState
from abc import ABC, abstractmethod


class RecordingError(Exception):
    """Exception generated when an error occurs recording results"""

    def __init__(self, message):
        super().__init__("Error recording results: " + message)


class Observer(ABC):

    @abstractmethod
    def update(self, player_state: PlayerState) -> None:
        raise NotImplementedError

    @abstractmethod
    @property
    def community(self) -> int:
        raise NotImplementedError

    @abstractmethod
    @property
    def generations(self) -> List[int]:
        raise NotImplementedError

    @abstractmethod
    def add_generation(self, generation: int) -> None:
        raise NotImplementedError

    @abstractmethod
    def add_player(self, generation: int, player: int) -> None:
        raise NotImplementedError

    @abstractmethod
    @property
    def corrupted_observations(self) -> bool:
        raise NotImplementedError


class ActionObserver(Observer):

    def __init__(self, community: int, generations: List[int] = None):
        self._community: int = community
        self._corrupted_observations: bool = False
        self._actions = []
        self._interactions = []
        self._cooperation_count = 0
        self._defection_count = 0
        self._positive_social_action_count = 0
        self._negative_social_action_count = 0
        self._idle_action_count = 0
        self._non_donor_action_count = 0
        if generations is not None:
            for i in range(len(generations)-1):
                for j in range(i+1, len(generations)):
                    if generations[i] == generations[j]:
                        self._corrupted_observations = True
                        raise RecordingError("Identical generation ids in constructor")
            self._players: Dict[int] = {generation: [] for generation in generations}
            self._generations: List[int] = generations
            self._actions_by_generation: Dict[int] = {generation: [] for generation in generations}
            self._actions_by_generation_and_player: Dict[int] = {generation: {} for generation in generations}
            self._interactions_by_generation: Dict[int] = {generation: [] for generation in generations}
            self._interactions_by_generation_and_player: Dict[int] = {generation: {} for generation in generations}
            self._cooperation_count_by_generation: Dict[int] = {generation: 0 for generation in generations}
            self._defection_count_by_generation: Dict[int] = {generation: 0 for generation in generations}
            self._cooperation_count_by_generation_and_player: Dict[int] = {generation: {} for generation in generations}
            self._defection_count_by_generation_and_player: Dict[int] = {generation: {} for generation in generations}
            self._positive_social_action_count_by_generation: Dict[int] = {generation: 0 for generation in generations}
            self._negative_social_action_count_by_generation: Dict[int] = {generation: 0 for generation in generations}
            self._idle_action_count_by_generation: Dict[int] = {generation: 0 for generation in generations}
            self._positive_social_action_count_by_generation_and_player: Dict[int] = \
                {generation: {} for generation in generations}
            self._negative_social_action_count_by_generation_and_player: Dict[int] = \
                {generation: {} for generation in generations}
            self._idle_action_count_by_generation_and_player: Dict[int] = {generation: {} for generation in generations}
            self._non_donor_action_count_by_generation: Dict[int] = {generation: 0 for generation in generations}
            self._non_donor_action_count_by_generation_and_player: Dict[int] = \
                {generation: {} for generation in generations}
        else:
            self._generations: List[int] = []
            self._players: Dict[int] = {}
            self._actions_by_generation: Dict[int] = {}
            self._actions_by_generation_and_player: Dict[int] = {}
            self._interactions_by_generation: Dict[int] = {}
            self._interactions_by_generation_and_player: Dict[int] = {}
            self._cooperation_count_by_generation: Dict[int] = {}
            self._defection_count_by_generation: Dict[int] = {}
            self._cooperation_count_by_generation_and_player: Dict[int] = {}
            self._defection_count_by_generation_and_player: Dict[int] = {}
            self._positive_social_action_count_by_generation: Dict[int] = {}
            self._negative_social_action_count_by_generation: Dict[int] = {}
            self._idle_action_count_by_generation: Dict[int] = {}
            self._positive_social_action_count_by_generation_and_player: Dict[int] = {}
            self._negative_social_action_count_by_generation_and_player: Dict[int] = {}
            self._idle_action_count_by_generation_and_player: Dict[int] = {}
            self._non_donor_action_count_by_generation: Dict[int] = {}
            self._non_donor_action_count_by_generation_and_player: Dict[int] = {}

    @property
    def corrupted_observations(self) -> bool:
        return self._corrupted_observations

    @property
    def community(self) -> int:
        return self._community

    @property
    def generations(self) -> List[int]:
        return self._generations

    def add_generation(self, generation: int) -> None:
        if generation in self._generations or generation in self._players:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add identical generation id in append")
        self._generations.append(generation)
        self._players[generation] = []
        self._actions_by_generation[generation] = []
        self._actions_by_generation_and_player[generation] = {}
        self._interactions_by_generation[generation] = []
        self._interactions_by_generation_and_player[generation] = {}
        self._cooperation_count_by_generation[generation] = 0
        self._defection_count_by_generation[generation] = 0
        self._cooperation_count_by_generation_and_player[generation] = {}
        self._defection_count_by_generation_and_player[generation] = {}
        self._positive_social_action_count_by_generation[generation] = 0
        self._negative_social_action_count_by_generation[generation] = 0
        self._idle_action_count_by_generation[generation] = 0
        self._positive_social_action_count_by_generation[generation] = {}
        self._negative_social_action_count_by_generation[generation] = {}
        self._idle_action_count_by_generation_and_player[generation] = {}
        self._non_donor_action_count_by_generation[generation] = 0
        self._non_donor_action_count_by_generation_and_player[generation] = {}

    @property
    def players(self) -> Dict[int]:
        return self._players

    def add_player(self, generation: int, player: int) -> None:
        if generation not in self._generations or generation not in self._players:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add a player to a non-existent generation")
        self._players[generation].append(player)
        self._actions_by_generation_and_player[generation][player] = []
        self._interactions_by_generation_and_player[generation][player] = []
        self._cooperation_count_by_generation_and_player[generation][player] = 0
        self._defection_count_by_generation_and_player[generation][player] = 0
        self._idle_action_count_by_generation_and_player[generation][player] = 0
        self._positive_social_action_count_by_generation_and_player[generation][player] = 0
        self._negative_social_action_count_by_generation_and_player[generation][player] = 0
        self._non_donor_action_count_by_generation_and_player[generation][player] = 0

    @property
    def actions(self):
        return self._actions

    @property
    def actions_by_generation(self):
        return self._actions_by_generation

    @property
    def actions_by_generation_and_player(self):
        return self._actions_by_generation_and_player

    @property
    def interactions(self):
        return self._interactions

    @property
    def interactions_by_generation(self):
        return self._interactions_by_generation

    @property
    def interactions_by_generation_and_player(self):
        return self._interactions_by_generation_and_player

    def update(self, player_state: PlayerState) -> None:
        self._update_actions(player_state.new_action)

    def _update_actions(self, action):
        if Action not in action.__class__.__mro__:
            raise RecordingError("Attempted to add an action that does not subclass from the Action subclass")
        if action.generation not in self._generations:
            raise RecordingError("Attempted to add an action that cannot be attributed to a generation, or player")
        if action.actor not in self._players[action.generation]:
            raise RecordingError("Attempted to add an action that cannot be attributed to an existing player")
        self._actions.append(action)
        self._actions_by_generation[action.generation].append(action)
        self._actions_by_generation_and_player[action.generation][action.actor].append(action)
        if action.type is ActionType.INTERACTION:
            self._interactions.append(action)
            self._interactions_by_generation[action.generation].append(action)
            self._interactions_by_generation_and_player[action.generation][action.actor].append(action)
            if action.action == InteractionContent.COOPERATE:
                self._cooperation_count += 1
                self._cooperation_count_by_generation[action.generation] += 1
                self._cooperation_count_by_generation_and_player[action.generation][action.actor] += 1
            else:
                self._defection_count += 1
                self._defection_count_by_generation[action.generation] += 1
                self._defection_count_by_generation_and_player[action.generation][action.actor] += 1
        elif action.type is ActionType.GOSSIP:
            if action.gossip is GossipContent.POSITIVE:
                self._positive_social_action_count += 1
                self._positive_social_action_count_by_generation[action.generation] += 1
                self._positive_social_action_count_by_generation_and_player[action.generation][action.actor] += 1
            elif action.gossip is GossipContent.NEGATIVE:
                self._negative_social_action_count += 1
                self._negative_social_action_count_by_generation[action.generation] += 1
                self._negative_social_action_count_by_generation_and_player[action.generation][action.actor] += 1

    @property
    def cooperation_rate(self) -> int:
        if self._cooperation_count + self._defection_count != 0:
            return int(round(100*(self._cooperation_count / (self._cooperation_count + self._defection_count))))
        return 0

    @property
    def cooperation_rate_by_generation(self) -> Dict[int]:
        cooperation_rate_by_generation: Dict[int] = {}
        for generation in self._generations:
            if self._cooperation_count_by_generation[generation] + self._defection_count_by_generation[generation] != 0:
                cooperation_rate_by_generation[generation] = int(round(100*(
                        self._cooperation_count_by_generation[generation] /
                        (self._cooperation_count_by_generation[generation] +
                         self._defection_count_by_generation[generation]))))
            else:
                cooperation_rate_by_generation[generation] = 0
        return cooperation_rate_by_generation

    @property
    def cooperation_rate_by_generation_and_player(self) -> Dict[int]:
        cooperation_rate_by_generation_and_player: Dict[int] = {}
        for generation in self._generations:
            cooperation_rate_by_generation_and_player[generation] = {}
            for player in self._players[generation]:
                if self._cooperation_count_by_generation_and_player[generation][player] +\
                        self._defection_count_by_generation_and_player[generation][player] != 0:
                    cooperation_rate_by_generation_and_player[generation][player] = int(round(100*(
                            self._cooperation_count_by_generation_and_player[generation][player] /
                            (self._cooperation_count_by_generation_and_player[generation][player] +
                             self._defection_count_by_generation_and_player[generation][player]))))
                else:
                    cooperation_rate_by_generation_and_player[generation][player] = 0
        return cooperation_rate_by_generation_and_player

    @property
    def social_activeness(self):
        if self._non_donor_action_count != 0:
            return int(round(100*((self._positive_social_action_count + self._negative_social_action_count)/
                                  self._non_donor_action_count)))
        return 0

    @property
    def social_activeness_by_generation(self) -> Dict[int]:
        social_activeness_by_generation: Dict[int] = {}
        for generation in self._generations:
            if self._non_donor_action_count_by_generation[generation] != 0:
                social_activeness_by_generation[generation] = int(round(100*(
                        (self._positive_social_action_count_by_generation[generation] +
                         self._negative_social_action_count_by_generation[generation]) /
                        self._non_donor_action_count_by_generation[generation])))
            else:
                social_activeness_by_generation[generation] = 0
        return social_activeness_by_generation

    @property
    def social_activeness_by_generation_and_player(self) -> Dict[int]:
        social_activeness_by_generation_and_player: Dict[int] = {}
        for generation in self._generations:
            social_activeness_by_generation_and_player[generation] = {}
            for player in self._players[generation]:
                if self._non_donor_action_count_by_generation_and_player[generation][player] != 0:
                    social_activeness_by_generation_and_player[generation][player] = int(round(100*(
                            (self._positive_social_action_count_by_generation_and_player[generation][player] +
                             self._negative_social_action_count_by_generation_and_player[generation][player]) /
                            (self._non_donor_action_count_by_generation_and_player[generation][player]))))
                else:
                    social_activeness_by_generation_and_player[generation][player] = 0
        return social_activeness_by_generation_and_player

    @property
    def positivity_of_gossip_percentage(self) -> int:
        if self._positive_social_action_count + self._negative_social_action_count != 0:
            return int(round(100*(self._positive_social_action_count /
                                  (self._positive_social_action_count + self._negative_social_action_count))))
        return 0

    @property
    def positivity_of_gossip_percentage_by_generation(self) -> Dict[int]:
        positivity_of_gossip_percentage_by_generation: Dict[int] = {}
        for generation in self._generations:
            if (self._positive_social_action_count_by_generation[generation] +
            self._negative_social_action_count_by_generation[generation]) != 0:
                positivity_of_gossip_percentage_by_generation[generation] = int(round(100*(
                        self._positive_social_action_count_by_generation[generation] /
                        (self._positive_social_action_count_by_generation[generation] +
                         self._negative_social_action_count_by_generation[generation]))))
            else:
                positivity_of_gossip_percentage_by_generation[generation] = 0
        return positivity_of_gossip_percentage_by_generation

    @property
    def positivity_of_gossip_percentage_by_generation_and_player(self) -> Dict[int]:
        positivity_of_gossip_percentage_by_generation_and_player: Dict[int] = {}
        for generation in self._generations:
            positivity_of_gossip_percentage_by_generation_and_player[generation] = {}
            for player in self._players[generation]:
                if (self._positive_social_action_count_by_generation_and_player[generation][player] +
                self._negative_social_action_count_by_generation_and_player[generation][player]) != 0:
                    positivity_of_gossip_percentage_by_generation_and_player[generation][player] = int(round(100*(
                        self._positive_social_action_count_by_generation_and_player[generation][player] /
                        (self._positive_social_action_count_by_generation_and_player[generation][player] +
                         self._negative_social_action_count_by_generation_and_player[generation][player])
                    )))
                else:
                    positivity_of_gossip_percentage_by_generation_and_player[generation][player] = 0
        return positivity_of_gossip_percentage_by_generation_and_player


class PlayerObserver(Observer):

    def __init__(self, community: int, generations: List[int] = None):
        self._community = community
        self._corrupted_observations: bool = False
        self._community_fitness = 0
        if generations is not None:
            for i in range(len(generations)-1):
                for j in range(i+1, len(generations)):
                    if generations[i] == generations[j]:
                        self._corrupted_observations = True
                        raise RecordingError("Identical generation ids in constructor")
            self._players: Dict[int] = {generation: [] for generation in generations}
            self._generations: List[int] = generations
            self._fitness_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._fitness_by_generation_and_player: Dict[int, Dict[int, int]] = {generation: {} for generation in
                                                                                 generations}
        else:
            self._generations: List[int] = []
            self._players: Dict[int] = {}
            self._fitness_by_generation: Dict[int, int] = {}
            self._fitness_by_generation_and_player: Dict[int, Dict[int, int]] = {}

    @property
    def corrupted_observations(self) -> bool:
        return self._corrupted_observations

    @property
    def community(self) -> int:
        return self._community

    @property
    def generations(self) -> List[int]:
        return self._generations

    def add_generation(self, generation: int) -> None:
        if generation in self._generations or generation in self._players:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add identical generation id in append")
        self._generations.append(generation)
        self._players[generation] = []
        self._fitness_by_generation[generation] = 0
        self._fitness_by_generation_and_player[generation] = {}

    def add_player(self, generation: int, player: int) -> None:
        if generation not in self._generations or generation not in self._players:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add a player to a non-existent generation")
        self._players[generation].append(player)
        self._fitness_by_generation_and_player[generation][player] = 0

    def update(self, player_state: PlayerState) -> None:
        self._update_fitness(player_state.fitness_update, player_state.generation, player_state.player)

    def _update_fitness(self, fitness_update: int, generation: int, player:int):
        self._community_fitness += fitness_update
        self._fitness_by_generation[generation] += fitness_update
        self._fitness_by_generation_and_player[generation][player] += fitness_update

    @property
    def community_fitness(self) -> int:
        return self._community_fitness

    @property
    def fitness_by_generation(self) -> Dict[int, int]:
        return self._fitness_by_generation

    @property
    def fitness_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        return self._fitness_by_generation_and_player


class Results:

    def __init__(self, community: Community):
        self._action_observer = ActionObserver(community.get_id())
        self._player_observer = PlayerObserver(community.get_id())
        self._observers: List[Observer] = [self._action_observer, self._player_observer]
        self._community = community

    @property
    def community(self) -> Community:
        return self._community

    @property
    def generations(self) -> List[int]:
        return self._action_observer.generations

    @property
    def observers(self) -> List[Observer]:
        return self._observers

    @property
    def actions(self):
        return self._action_observer.actions

    @property
    def actions_by_generation(self):
        return self._action_observer.actions_by_generation

    @property
    def actions_by_generation_and_player(self):
        return self._action_observer.actions_by_generation_and_player

    @property
    def interactions(self):
        return self._action_observer.interactions

    @property
    def interactions_by_generation(self):
        return self._action_observer.interactions_by_generation

    @property
    def interactions_by_generation_and_player(self):
        return self._action_observer.interactions_by_generation_and_player

    @property
    def cooperation_rate(self) -> int:
        return self._action_observer.cooperation_rate

    @property
    def cooperation_rate_by_generation(self) -> Dict[int]:
        return self._action_observer.cooperation_rate_by_generation

    @property
    def cooperation_rate_by_generation_and_player(self) -> Dict[int]:
        return self._action_observer.cooperation_rate_by_generation_and_player

    @property
    def social_activeness(self):
        return self._action_observer.social_activeness

    @property
    def social_activeness_by_generation(self) -> Dict[int]:
        return self._action_observer.social_activeness_by_generation

    @property
    def social_activeness_by_generation_and_player(self) -> Dict[int]:
        return self._action_observer.social_activeness_by_generation_and_player

    @property
    def positivity_of_gossip_percentage(self) -> int:
        return self._action_observer.positivity_of_gossip_percentage

    @property
    def positivity_of_gossip_percentage_by_generation(self) -> Dict[int]:
        return self._action_observer.positivity_of_gossip_percentage_by_generation

    @property
    def positivity_of_gossip_percentage_by_generation_and_player(self) -> Dict[int]:
        return self._action_observer.positivity_of_gossip_percentage_by_generation_and_player

    @property
    def corrupted_observations(self) -> bool:
        for observer in self._observers:
            if observer.corrupted_observations:
                return True
        return False

    @property
    def community_fitness(self) -> int:
        return self._player_observer.community_fitness

    @property
    def fitness_by_generation(self) -> Dict[int, int]:
        return self._player_observer.fitness_by_generation

    @property
    def fitness_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        return self._player_observer.fitness_by_generation_and_player

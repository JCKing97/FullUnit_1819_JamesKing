"""observation_logic.py: Contains the logic to record the results and statistics on a community simulation"""

__author__ = "James King"

from .action_logic import Action, ActionType, InteractionContent, GossipContent, InteractionAction
from typing import List, Dict
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

    @property
    @abstractmethod
    def community(self) -> int:
        raise NotImplementedError

    @property
    @abstractmethod
    def generations(self) -> List[int]:
        raise NotImplementedError

    @abstractmethod
    def add_generation(self, generation: int) -> None:
        raise NotImplementedError

    @abstractmethod
    def add_player(self, generation: int, player: int) -> None:
        raise NotImplementedError

    @property
    @abstractmethod
    def corrupted_observations(self) -> bool:
        raise NotImplementedError


class ActionObserver(Observer):

    def __init__(self, community: int, generations: List[int] = None):
        self._community: int = community
        self._corrupted_observations: bool = False
        self._actions: Dict[int, List[Action]] = {}
        self._interactions: Dict[int, List[Action]] = {}
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
            self._players: Dict[int, List[int]] = {generation: [] for generation in generations}
            self._generations: List[int] = generations
            self._actions_by_generation: Dict[int, Dict[int, List[Action]]] = {generation: {} for generation in generations}
            self._actions_by_generation_and_player: Dict[int, Dict[int, Dict[int, List[Action]]]] = {generation: {} for generation in generations}
            self._interactions_by_generation: Dict[int, Dict[int, List[InteractionAction]]] = {generation: {} for generation in generations}
            self._interactions_by_generation_and_player: Dict[int, Dict[int, Dict[int, List[InteractionAction]]]] = {generation: {} for generation in generations}
            self._cooperation_count_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._defection_count_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._cooperation_count_by_generation_and_player: Dict[int, Dict[int, int]] = {generation: {} for generation in generations}
            self._defection_count_by_generation_and_player: Dict[int, Dict[int, int]] = {generation: {} for generation in generations}
            self._positive_social_action_count_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._negative_social_action_count_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._idle_action_count_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._positive_social_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = \
                {generation: {} for generation in generations}
            self._negative_social_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = \
                {generation: {} for generation in generations}
            self._idle_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = {generation: {} for generation in generations}
            self._non_donor_action_count_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._non_donor_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = \
                {generation: {} for generation in generations}
        else:
            self._generations: List[int] = []
            self._players: Dict[int, List[int]] = {}
            self._actions_by_generation: Dict[int, Dict[int, List[Action]]] = {}
            self._actions_by_generation_and_player: Dict[int, Dict[int, Dict[int, List[Action]]]] = {}
            self._interactions_by_generation: Dict[int, Dict[int, List[InteractionAction]]] = {}
            self._interactions_by_generation_and_player: Dict[int, Dict[int, Dict[int, List[InteractionAction]]]] = {}
            self._cooperation_count_by_generation: Dict[int, int] = {}
            self._defection_count_by_generation: Dict[int, int] = {}
            self._cooperation_count_by_generation_and_player: Dict[int, Dict[int, int]] = {}
            self._defection_count_by_generation_and_player: Dict[int, Dict[int, int]] = {}
            self._positive_social_action_count_by_generation: Dict[int, int] = {}
            self._negative_social_action_count_by_generation: Dict[int, int] = {}
            self._idle_action_count_by_generation: Dict[int, int] = {}
            self._positive_social_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = {}
            self._negative_social_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = {}
            self._idle_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = {}
            self._non_donor_action_count_by_generation: Dict[int, int] = {}
            self._non_donor_action_count_by_generation_and_player: Dict[int, Dict[int, int]] = {}

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
        self._actions_by_generation[generation] = {}
        self._actions_by_generation_and_player[generation] = {}
        self._interactions_by_generation[generation] = {}
        self._interactions_by_generation_and_player[generation] = {}
        self._cooperation_count_by_generation[generation] = 0
        self._defection_count_by_generation[generation] = 0
        self._cooperation_count_by_generation_and_player[generation] = {}
        self._defection_count_by_generation_and_player[generation] = {}
        self._positive_social_action_count_by_generation[generation] = 0
        self._negative_social_action_count_by_generation[generation] = 0
        self._idle_action_count_by_generation[generation] = 0
        self._positive_social_action_count_by_generation_and_player[generation] = {}
        self._negative_social_action_count_by_generation_and_player[generation] = {}
        self._idle_action_count_by_generation_and_player[generation] = {}
        self._non_donor_action_count_by_generation[generation] = 0
        self._non_donor_action_count_by_generation_and_player[generation] = {}

    @property
    def players(self) -> Dict[int, List[int]]:
        return self._players

    def add_player(self, generation: int, player: int) -> None:
        if generation not in self._generations or generation not in self._players or \
                (generation in self._players and player in self.players[generation]):
            self._corrupted_observations = True
            raise RecordingError("Attempted to add a player to a non-existent generation")
        self._players[generation].append(player)
        self._actions_by_generation_and_player[generation][player] = {}
        self._interactions_by_generation_and_player[generation][player] = {}
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
        if player_state.new_action is not None:
            self._update_actions(player_state.new_action)

    def _update_actions(self, action):
        if Action not in action.__class__.__mro__:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add an action that does not subclass from the Action subclass")
        if action.generation not in self._generations:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add an action that cannot be attributed to a generation, or player")
        if action.actor not in self._players[action.generation]:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add an action that cannot be attributed to an existing player")
        self._add_action(action)
        if action.type is ActionType.INTERACTION:
            self._add_interaction(action)
            if action.action == InteractionContent.COOPERATE:
                self._cooperation_count += 1
                self._cooperation_count_by_generation[action.generation] += 1
                self._cooperation_count_by_generation_and_player[action.generation][action.actor] += 1
            else:
                self._defection_count += 1
                self._defection_count_by_generation[action.generation] += 1
                self._defection_count_by_generation_and_player[action.generation][action.actor] += 1
        elif action.type is ActionType.GOSSIP:
            self._non_donor_action_count += 1
            self._non_donor_action_count_by_generation[action.generation] += 1
            self._non_donor_action_count_by_generation_and_player[action.generation][action.actor] += 1
            if action.gossip is GossipContent.POSITIVE:
                self._positive_social_action_count += 1
                self._positive_social_action_count_by_generation[action.generation] += 1
                self._positive_social_action_count_by_generation_and_player[action.generation][action.actor] += 1
            elif action.gossip is GossipContent.NEGATIVE:
                self._negative_social_action_count += 1
                self._negative_social_action_count_by_generation[action.generation] += 1
                self._negative_social_action_count_by_generation_and_player[action.generation][action.actor] += 1
        else:
            self._non_donor_action_count += 1
            self._non_donor_action_count_by_generation[action.generation] += 1
            self._non_donor_action_count_by_generation_and_player[action.generation][action.actor] += 1

    def _add_action(self, action):
        if action.timepoint in self._actions:
            self._actions[action.timepoint].append(action)
        else:
            self._actions[action.timepoint] = [action]
        if action.timepoint in self._actions_by_generation[action.generation]:
            self._actions_by_generation[action.generation][action.timepoint].append(action)
        else:
            self._actions_by_generation[action.generation][action.timepoint] = [action]
        if action.timepoint in self._actions_by_generation_and_player[action.generation][action.actor]:
            self._actions_by_generation_and_player[action.generation][action.actor][action.timepoint].append(action)
        else:
            self._actions_by_generation_and_player[action.generation][action.actor][action.timepoint] = [action]

    def _add_interaction(self, action):
        if action.timepoint in self._interactions:
            self._interactions[action.timepoint].append(action)
        else:
            self._interactions[action.timepoint] = [action]
        if action.timepoint in self._interactions_by_generation[action.generation]:
            self._interactions_by_generation[action.generation][action.timepoint].append(action)
        else:
            self._interactions_by_generation[action.generation][action.timepoint] = [action]
        if action.timepoint in self._interactions_by_generation_and_player[action.generation][action.actor]:
            self._interactions_by_generation_and_player[action.generation][action.actor][action.timepoint]. \
                append(action)
        else:
            self._interactions_by_generation_and_player[action.generation][action.actor][action.timepoint] = \
                [action]

    @property
    def cooperation_rate(self):
        if self._cooperation_count + self._defection_count != 0:
            return int(round(100*(self._cooperation_count / (self._cooperation_count + self._defection_count))))
        return None

    @property
    def cooperation_rate_by_generation(self) -> Dict[int, int]:
        cooperation_rate_by_generation: Dict[int, int] = {}
        for generation in self._generations:
            if self._cooperation_count_by_generation[generation] + self._defection_count_by_generation[generation] != 0:
                cooperation_rate_by_generation[generation] = int(round(100*(
                        self._cooperation_count_by_generation[generation] /
                        (self._cooperation_count_by_generation[generation] +
                         self._defection_count_by_generation[generation]))))
            else:
                cooperation_rate_by_generation[generation] = None
        return cooperation_rate_by_generation

    @property
    def cooperation_rate_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        cooperation_rate_by_generation_and_player: Dict[int, Dict[int, int]] = {}
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
                    cooperation_rate_by_generation_and_player[generation][player] = None
        return cooperation_rate_by_generation_and_player

    @property
    def social_activeness(self):
        if self._non_donor_action_count != 0:
            return int(round(100*((self._positive_social_action_count + self._negative_social_action_count)/
                                  self._non_donor_action_count)))
        return None

    @property
    def social_activeness_by_generation(self) -> Dict[int, int]:
        social_activeness_by_generation: Dict[int, int] = {}
        for generation in self._generations:
            if self._non_donor_action_count_by_generation[generation] != 0:
                social_activeness_by_generation[generation] = int(round(100*(
                        (self._positive_social_action_count_by_generation[generation] +
                         self._negative_social_action_count_by_generation[generation]) /
                        self._non_donor_action_count_by_generation[generation])))
            else:
                social_activeness_by_generation[generation] = None
        return social_activeness_by_generation

    @property
    def social_activeness_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        social_activeness_by_generation_and_player: Dict[int, Dict[int, int]] = {}
        for generation in self._generations:
            social_activeness_by_generation_and_player[generation] = {}
            for player in self._players[generation]:
                if self._non_donor_action_count_by_generation_and_player[generation][player] != 0:
                    social_activeness_by_generation_and_player[generation][player] = int(round(100*(
                            (self._positive_social_action_count_by_generation_and_player[generation][player] +
                             self._negative_social_action_count_by_generation_and_player[generation][player]) /
                            (self._non_donor_action_count_by_generation_and_player[generation][player]))))
                else:
                    social_activeness_by_generation_and_player[generation][player] = None
        return social_activeness_by_generation_and_player

    @property
    def positivity_of_gossip_percentage(self):
        if self._positive_social_action_count + self._negative_social_action_count != 0:
            return int(round(100*(self._positive_social_action_count /
                                  (self._positive_social_action_count + self._negative_social_action_count))))
        return None

    @property
    def positivity_of_gossip_percentage_by_generation(self) -> Dict[int, int]:
        positivity_of_gossip_percentage_by_generation: Dict[int, int] = {}
        for generation in self._generations:
            if (self._positive_social_action_count_by_generation[generation] +
            self._negative_social_action_count_by_generation[generation]) != 0:
                positivity_of_gossip_percentage_by_generation[generation] = int(round(100*(
                        self._positive_social_action_count_by_generation[generation] /
                        (self._positive_social_action_count_by_generation[generation] +
                         self._negative_social_action_count_by_generation[generation]))))
            else:
                positivity_of_gossip_percentage_by_generation[generation] = None
        return positivity_of_gossip_percentage_by_generation

    @property
    def positivity_of_gossip_percentage_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        positivity_of_gossip_percentage_by_generation_and_player: Dict[int, Dict[int, int]] = {}
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
                    positivity_of_gossip_percentage_by_generation_and_player[generation][player] = None
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
            self._players: Dict[int, List[int]] = {generation: [] for generation in generations}
            self._generations: List[int] = generations
            self._fitness_by_generation: Dict[int, int] = {generation: 0 for generation in generations}
            self._fitness_by_generation_and_player: Dict[int, Dict[int, int]] = {generation: {} for generation in
                                                                                 generations}
        else:
            self._generations: List[int] = []
            self._players: Dict[int, List[int]] = {}
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

    @property
    def players(self) -> Dict[int, List[int]]:
        return self._players

    def add_player(self, generation: int, player: int) -> None:
        if generation not in self._generations or generation not in self._players or \
                (generation in self._players and player in self.players[generation]):
            self._corrupted_observations = True
            raise RecordingError("Attempted to add a player to a non-existent generation")
        self._players[generation].append(player)
        self._fitness_by_generation_and_player[generation][player] = 0

    def update(self, player_state: PlayerState) -> None:
        if player_state.generation not in self._generations or player_state.generation not in self.players or (
                player_state.generation in self._players and player_state.player
                not in self._players[player_state.generation]):
            self._corrupted_observations = True
            raise RecordingError("Tried to update a player state of a player not added to the observer")
        self._update_fitness(player_state.fitness_update, player_state.generation, player_state.player)

    def _update_fitness(self, fitness_update: int, generation: int, player: int) -> None:
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

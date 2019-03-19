"""observation_logic.py: Contains the logic to record the results and statistics on a community simulation"""

__author__ = "James King"

from .action_logic import Action, ActionType, InteractionContent, GossipContent, InteractionAction
from typing import List, Dict, NoReturn, Union
from .player_logic import PlayerState
from abc import ABC, abstractmethod


class RecordingError(Exception):
    """Exception generated when an error occurs recording results"""

    def __init__(self, message):
        super().__init__("Error recording results: " + message)


class Observer(ABC):
    """Ensure these methods are implemented for every subclassing observer"""

    @abstractmethod
    def update(self, player_state: PlayerState) -> None:
        """Update the observer with new player state information"""
        raise NotImplementedError

    @property
    @abstractmethod
    def community(self) -> int:
        """The id for the community this is an observer of (Not Implemented Here)"""
        raise NotImplementedError

    @property
    @abstractmethod
    def generations(self) -> List[int]:
        """A list of generation ids of the community this is observing (Not Implemented Here)"""
        raise NotImplementedError

    @abstractmethod
    def add_generation(self, generation: int) -> NoReturn:
        """Add a generation to observer"""
        raise NotImplementedError

    @abstractmethod
    def add_player(self, generation: int, player: int) -> NoReturn:
        """Add a player to observer"""
        raise NotImplementedError

    @property
    @abstractmethod
    def corrupted_observations(self) -> bool:
        """Return if the observations of this observer are corrupted in some way"""
        raise NotImplementedError


class ActionObserver(Observer):
    """Observes the actions committed to by players of a community"""

    def __init__(self, community: int, generations: List[int] = None):
        """
        Set up the relevant data structures to store observations in
        :param community: The id of the community to observer
        :type community: int
        :param generations: A list of ids of the generations that have been simulated in this community
        """
        # Community level data set up
        self._community: int = community
        self._corrupted_observations: bool = False
        self._actions: Dict[int, List[Action]] = {}
        self._interactions: Dict[int, InteractionAction] = {}
        self._cooperation_count = 0
        self._defection_count = 0
        self._positive_social_action_count = 0
        self._negative_social_action_count = 0
        self._idle_action_count = 0
        self._non_donor_action_count = 0
        # Generational and player level data set up
        if generations is not None:
            for i in range(len(generations)-1):
                for j in range(i+1, len(generations)):
                    if generations[i] == generations[j]:
                        self._corrupted_observations = True
                        raise RecordingError("Identical generation ids in constructor")
            self._players: Dict[int, List[int]] = {generation: [] for generation in generations}
            self._generations: List[int] = generations
            self._actions_by_generation: Dict[int, Dict[int, List[Action]]] = {generation: {} for generation in generations}
            self._actions_by_generation_and_player: Dict[int, Dict[int, Dict[int, Action]]] = {generation: {} for generation in generations}
            self._interactions_by_generation: Dict[int, Dict[int, InteractionAction]] = {generation: {} for generation in generations}
            self._interactions_by_generation_and_player: Dict[int, Dict[int, Dict[int, InteractionAction]]] = {generation: {} for generation in generations}
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
            self._actions_by_generation_and_player: Dict[int, Dict[int, Dict[int, Action]]] = {}
            self._interactions_by_generation: Dict[int, Dict[int, InteractionAction]] = {}
            self._interactions_by_generation_and_player: Dict[int, Dict[int, Dict[int, InteractionAction]]] = {}
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
        """
        Have the observations been corrupted in some way?
        :return: Are the observations corrupted
        :rtype: bool
        """
        return self._corrupted_observations

    @property
    def community(self) -> int:
        """
        Get the id of the community this observer is observing
        :return: id of community observing
        :rtype: int
        """
        return self._community

    @property
    def generations(self) -> List[int]:
        """
        Ids of generations this observer is observing
        :return: generation ids observing
        :rtype: List[int]
        """
        return self._generations

    def add_generation(self, generation: int) -> NoReturn:
        """
        Update the generational level and player level data structures with a new generation
        :param generation: the id of the generation to add
        :type generation: int
        :return: NoReturn
        """
        # Detect if observations have become corrupted
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
        """
        Get the id's of players this observer is observing as a dictionary where keys are generation ids that point
        to a list of player ids for that generation
        :return: The id's of the players this observer is observing
        :rtype: Dict[int, List[int]]
        """
        return self._players

    def add_player(self, generation: int, player: int) -> NoReturn:
        """
        Update player level data structures with a new player to observer
        :param generation: the id of the generation the player belongs to
        :type generation: int
        :param player: the id of the player to add
        :type player: int
        :return: NoReturn
        """
        # Detect if corruption has occurred
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
    def actions(self) -> Dict[int, List[Action]]:
        """
        Get a list of actions for each timepoint
        :return: list of actions for each timepoint
        :rtype: Dict[int, List[Action]]
        """
        return self._actions

    @property
    def actions_by_generation(self) -> Dict[int, Dict[int, List[Action]]]:
        """
        Get lists of actions indexed by generation at the first level and timepoint at the second level
        :return: List of actions organised by generation and timepoint
        :rtype: Dict[int, Dict[int, List[Action]]]
        """
        return self._actions_by_generation

    @property
    def actions_by_generation_and_player(self) -> Dict[int, Dict[int, Dict[int, Action]]]:
        """
        Get actions for each player at every timepoint that they acted as a dictionary, where the first level of keys
        is the generation id, the second is the player id and the third is the timepoint
        :return: Actions for each player at each timepoint that they acted
        :rtype: Dict[int, Dict[int, Dict[int, Action]]]
        """
        return self._actions_by_generation_and_player

    @property
    def interactions(self) -> Dict[int, InteractionAction]:
        """
        Get the donor action take for each interaction interactions that occurred at each timepoint (with the index as
        the timepoint)
        :return: Interactions in the community indexed by timepoint
        :rtype: Dict[int, InteractionAction]
        """
        return self._interactions

    @property
    def interactions_by_generation(self) -> Dict[int, Dict[int, InteractionAction]]:
        """
        Get the interactions ordered by generation at the first level of dictionary keys and timepoint at the next
        level of dictionary keys that point to the donors actions in those interactions
        :return: interactions indexed by generation
        :rtype: Dict[int, Dict[int, InteractionAction]]
        """
        return self._interactions_by_generation

    @property
    def interactions_by_generation_and_player(self) -> Dict[int, Dict[int, Dict[int, InteractionAction]]]:
        """
        Get the interactions indexed by generation and player ids, with the last key level being the timepoint
        :return: Interactions indexed by generation and player
        :rtype: Dict[int, Dict[int, Dict[int, InteractionAction]]]
        """
        return self._interactions_by_generation_and_player

    def update(self, player_state: PlayerState) -> NoReturn:
        """
        A new event has occurred to change the state of the player, observe this and update observations accordingly
        :param player_state: The state of the player containg the new action
        :type: PlayerState
        :return: NoReturn
        """
        if player_state.new_action is not None:
            self._update_actions(player_state.new_action)

    def _update_actions(self, action) -> NoReturn:
        """
        A new action has been taken update observations
        :param action: The new action
        :type: Action
        :return: NoReturn
        """
        # Detect possible corruptions
        if Action not in action.__class__.__mro__:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add an action that does not subclass from the Action subclass")
        if action.generation not in self._generations:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add an action that cannot be attributed to a generation, or player")
        if action.actor not in self._players[action.generation]:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add an action that cannot be attributed to an existing player")
        # Add action to action list
        self._add_action(action)
        if action.type is ActionType.INTERACTION:
            # Record relevant data on interactions
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
            # Record relevant data on gossip actions
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
            # Record inactivity
            self._non_donor_action_count += 1
            self._non_donor_action_count_by_generation[action.generation] += 1
            self._non_donor_action_count_by_generation_and_player[action.generation][action.actor] += 1

    def _add_action(self, action) -> NoReturn:
        """
        Add the action to the relevant data structures
        :param action: The action to record
        :type action: Action
        :return: NoReturn
        """
        # Add action to relevant timepoint
        if action.timepoint in self._actions:
            self._actions[action.timepoint].append(action)
        else:
            self._actions[action.timepoint] = [action]
        # Add to relevant generation timepoint
        if action.timepoint in self._actions_by_generation[action.generation]:
            self._actions_by_generation[action.generation][action.timepoint].append(action)
        else:
            self._actions_by_generation[action.generation][action.timepoint] = [action]
        # Set players action for the timepoint
        self._actions_by_generation_and_player[action.generation][action.actor][action.timepoint] = action

    def _add_interaction(self, action) -> NoReturn:
        """
        Add the interaction data to the relevant data structures
        :param action: the interaction action to add
        :type action: InteractionAction
        :return: NoReturn
        """
        self._interactions[action.timepoint] = action
        self._interactions_by_generation[action.generation][action.timepoint] = action
        self._interactions_by_generation_and_player[action.generation][action.actor][action.timepoint] = action

    @property
    def cooperation_rate(self) -> Union[int, None]:
        """
        Get the cooperation rate of the generation or None if there has been no interactions recorded
        :return: The cooperation rate or none
        :rtype: Union[int, None]
        """
        if self._cooperation_count + self._defection_count != 0:
            return int(round(100*(self._cooperation_count / (self._cooperation_count + self._defection_count))))
        return None

    @property
    def cooperation_rate_by_generation(self) -> Dict[int, Union[int, None]]:
        """
        Get the cooperation rate of each generation, where the key is the generation id
        :return: cooperation rate of each generation
        :rtype: Dict[int, Union[int, None]]
        """
        cooperation_rate_by_generation: Dict[int, Union[int, None]] = {}
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
    def cooperation_rate_by_generation_and_player(self) -> Dict[int, Dict[int, Union[int, None]]]:
        """
        Get the cooperation rate of each player as a dictionary with the first level of keys being the generation id,
        the second level being the player id that points to the cooperation rate or None if the player has not been a
        donor
        :return: The cooperation rate of each player
        :rtype: Dict[int, Dict[int, Union[int, None]]]
        """
        cooperation_rate_by_generation_and_player: Dict[int, Dict[int, Union[int, None]]] = {}
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
    def social_activeness(self) -> Union[int, None]:
        """
        Get the social activeness of the community as a percentage of the donor actions that have been social or None
        if there has been no action
        :return: The social activeness of the community
        :rtype: Union[int, None]
        """
        if self._non_donor_action_count != 0:
            return int(round(100*((self._positive_social_action_count + self._negative_social_action_count)/
                                  self._non_donor_action_count)))
        return None

    @property
    def social_activeness_by_generation(self) -> Dict[int, Union[int, None]]:
        """
        Get the social activeness of each generation, which may be none if there have been no actions
        :return: the social activeness of each generation
        :rtype: Dict[int, Union[int, None]]
        """
        social_activeness_by_generation: Dict[int, Union[int, None]] = {}
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
    def social_activeness_by_generation_and_player(self) -> Dict[int, Dict[int, Union[int, None]]]:
        """
        Get the social activeness of each player as a dictionary where the first level of keys are the generation ids,
        the second level of keys are the player ids that point to the social activeness or None if they have not
        committed to any non donor actions
        :return: the social activeness of each player
        :rtype: Dict[int, Dict[int, Union[int, None]]]
        """
        social_activeness_by_generation_and_player: Dict[int, Dict[int, Union[int, None]]] = {}
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
    def positivity_of_gossip_percentage(self) -> Union[int, None]:
        """
        Get the positivity of the gossip across the whole community, or None if there have been no actions observed
        :return: The community's gossip positivity
        :rtype: Union[int, None]
        """
        if self._positive_social_action_count + self._negative_social_action_count != 0:
            return int(round(100*(self._positive_social_action_count /
                                  (self._positive_social_action_count + self._negative_social_action_count))))
        return None

    @property
    def positivity_of_gossip_percentage_by_generation(self) -> Dict[int, Union[int, None]]:
        """
        Get the positivity of the gossip of each generation, the keys of the returned dictionary are the generation
        ids, the values will be none if no gossip actions have occurred in that generation
        :return: the postivity of each generations gossip#
        :rtype: Dict[int, Union[int, None]]
        """
        positivity_of_gossip_percentage_by_generation: Dict[int, Union[int, None]] = {}
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
    def positivity_of_gossip_percentage_by_generation_and_player(self) -> Dict[int, Dict[int, Union[int, None]]]:
        """
        Get the positivity of each players gossip as a dictionary where the first layer of keys are the generation ids,
        the next layer are the player ids and the values are the percentage of gossip that has been positive or None
        if there has been no gossip actions committed by the player
        :return: the positivity of each players gossip
        :rtype: Dict[int, Dict[int, Union[int, None]]]
        """
        positivity_of_gossip_percentage_by_generation_and_player: Dict[int, Dict[int, Union[int, None]]] = {}
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
    """Observer of the condition of each player (fitness)"""

    def __init__(self, community: int, generations: List[int] = None):
        """
        Set up the data structures for the recording of observation data
        :param community: the id of the community to be observed
        :type community: int
        :param generations: A list of generation ids of the generations to observe
        :type generations: int
        """
        # Set up community level data structures
        self._community = community
        self._corrupted_observations: bool = False
        self._community_fitness = 0
        # Set up generation and player level data structures
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
        """
        Have the observations been corrupted?
        :return: have the observations been corrupted?
        :rtype: bool
        """
        return self._corrupted_observations

    @property
    def community(self) -> int:
        """
        The id of the community that is being observed
        :return: the id of the observed community
        :rtype: int
        """
        return self._community

    @property
    def generations(self) -> List[int]:
        """
        A list of the generation ids that are being observed
        :return: Observed generation's ids
        :rtype: List[int]
        """
        return self._generations

    def add_generation(self, generation: int) -> NoReturn:
        """
        Add a generation to observe, and fill in the correct data structures
        :param generation: The id of the generation to observe
        :type generation: int
        :return: NoReturn
        """
        # Detect observation corruption
        if generation in self._generations or generation in self._players:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add identical generation id in append")
        # Update relevant data structures
        self._generations.append(generation)
        self._players[generation] = []
        self._fitness_by_generation[generation] = 0
        self._fitness_by_generation_and_player[generation] = {}

    @property
    def players(self) -> Dict[int, List[int]]:
        """
        The ids of the players being observed stored as a dictionary where the key is the generation of those players
        that points to a lit of all the ids of the generation's players
        :return: player ids indexed by generation
        :rtype: Dict[int, List[int]]
        """
        return self._players

    def add_player(self, generation: int, player: int) -> NoReturn:
        """
        Add a player to observe, and update relevant data structures
        :param generation: The id of the generation the player belongs to
        :type generation: int
        :param player: The id of the player to observe
        :type player: int
        :return: NoReturn
        """
        # Detect corruption
        if generation not in self._generations or generation not in self._players or \
                (generation in self._players and player in self.players[generation]):
            self._corrupted_observations = True
            raise RecordingError("Attempted to add a player to a non-existent generation")
        # Update relevant data structures
        self._players[generation].append(player)
        self._fitness_by_generation_and_player[generation][player] = 0

    def update(self, player_state: PlayerState) -> NoReturn:
        """
        A new event has occurred to change the state of the player, read the players state to update observations
        :param player_state: The state of the player that may have changed
        :type player_state: PlayerState
        :return: NoReturn
        """
        # Detect corruption
        if player_state.generation not in self._generations or player_state.generation not in self.players or (
                player_state.generation in self._players and player_state.player
                not in self._players[player_state.generation]):
            self._corrupted_observations = True
            raise RecordingError("Tried to update a player state of a player not added to the observer")
        # Update relevant observations
        self._update_fitness(player_state.fitness_update, player_state.generation, player_state.player)

    def _update_fitness(self, fitness_update: int, generation: int, player: int) -> NoReturn:
        """
        Update the community, generation and player fitness based on a change in the players fitness
        :param fitness_update: The fitness update that occurred
        :type fitness_update: int
        :param generation: The id of the generation the player belongs to
        :type generation: int
        :param player: The id of the player
        :type player: int
        :return: NoReturn
        """
        self._community_fitness += fitness_update
        self._fitness_by_generation[generation] += fitness_update
        self._fitness_by_generation_and_player[generation][player] += fitness_update

    @property
    def community_fitness(self) -> int:
        """
        Get the summation of all the players' fitness in the community
        :return: sum of all players' fitness
        :rtype: int
        """
        return self._community_fitness

    @property
    def fitness_by_generation(self) -> Dict[int, int]:
        """
        Get the fitness of all the players in each generation as a dictionary where the keys are the generation ids
        :return: the sum of each generation's players' fitness
        :rtype: Dict[int, int]
        """
        return self._fitness_by_generation

    @property
    def fitness_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        """
        Get the fitness of each player as a dictionary where they first layer of keys are the generation ids, the
        second layer of keys are the respective generation's players' ids that point to the fitness of each player
        :return: The fitness of each player
        :rtype: Dict[int, Dict[int, int]]
        """
        return self._fitness_by_generation_and_player

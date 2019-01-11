"""results_logic.py: Contains the logic to record the results and statistics on a community simulation"""

__author__ = "James King"

from .action_logic import Action
from typing import List, Dict


class RecordingError(Exception):
    """Exception generated when an error occurs recording results"""

    def __init__(self, message):
        super().__init__("Error recording results: " + message)


class ActionObserver:

    def __init__(self, community: int, generations: List[int] = None):
        self._community: int = community
        self._corrupted_observations: bool = False
        self._actions = []
        if generations is not None:
            self._players: Dict[int] = {generation: [] for generation in generations}
            self._generations: List[int] = generations
            self._actions_by_generation: Dict[int] = {generation: [] for generation in generations}
            for i in range(len(generations)-1):
                for j in range(i+1, len(generations)):
                    if generations[i] == generations[j]:
                        self._corrupted_observations = True
                        raise RecordingError("Identical generation ids in constructor")
        else:
            self._generations: List[int] = []
            self._players: Dict[int] = {}
            self._actions_by_generation: Dict[int] = {}

    @property
    def corrupted_observations(self) -> bool:
        return self._corrupted_observations

    @property
    def community(self) -> int:
        return self._community

    @property
    def generations(self) -> List[int]:
        return self._generations

    def add_generation(self, generation: int):
        if generation in self._generations or generation in self._players:
            self._corrupted_observations = True
            raise RecordingError("Attempted to add identical generation id in append")
        self._generations.append(generation)
        self._players[generation] = []

    @property
    def players(self) -> Dict[int]:
        return self._players

    def add_player(self, generation: int, player: int):
        if generation not in self._generations or generation not in self._players:
            raise RecordingError("Attempted to add a player to a non-existent generation")
        self._players[generation].append(player)

    @property
    def actions(self):
        return self._actions

    @property
    def actions_by_generation(self):
        return self._actions_by_generation

    def update(self, action: Action):
        if action.generation not in self._generations:
            raise RecordingError("Attempted to add an action that cannot be attributed to a generation, or player")
        if action.actor not in self._players[action.generation]:
            raise RecordingError("Attempted to add an action that cannot be attributed to an existing player")
        self._actions.append(action)
        self._actions_by_generation[action.generation].append(action)

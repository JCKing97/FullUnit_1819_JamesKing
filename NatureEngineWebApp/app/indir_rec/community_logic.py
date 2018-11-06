"""community_logic.py: Contains the objects and logic for the generations of a community"""

from typing import Dict, List
from app.indir_rec.player_logic import Player, PlayerFactory
from app.indir_rec.action_logic import Action
from app.indir_rec.percept_logic import Percept, PerceptDonor, PerceptRecipient
import random
import copy


class Community:
    """A community that goes through a number of generations"""

    def __init__(self, onlooker_number: int, generation_length: int):
        self._generations = []
        self._current_time = 0
        self._onlooker_number = onlooker_number
        self._generation_length = generation_length


class Generation:
    """A generation of players inside a community"""

    def __init__(self, strategies: Dict[str, int], start_time: int, end_time: int, community: Community):
        self._players = {}
        for strategy, strategy_count in strategies.items():
            for i in range(strategy_count):
                new_player = PlayerFactory.new_player(strategy)
                self._players[new_player.get_id()] = new_player
        self._start_time = start_time
        self._end_time = end_time
        self._community = community
        self._actions = []
        self._new_percepts = []

    def id_to_player(self, id: int) -> Player:
        return self._players[id]

    def _generate_meeting_percept_and_onlookers(self):
        ids = copy.deepcopy(self._players.keys())
        donor_id = random.choice(ids)
        del ids[donor_id]
        recipient_id = random.choice(ids)
        onlooker_number = self._community.get_onlooker_number()
        onlookers = []
        for i in range(onlooker_number):
            if len(ids) > 0:
                onlooker_id = random.choice(ids)
                onlookers.append(self._players[onlooker_id])
            else:
                break
        return [PerceptDonor(self._players[donor_id]), PerceptRecipient(self._players[recipient_id])], onlookers

    def simulate(self):
        for current_time_point in range(self._start_time, self._end_time):
            # Perceive
            for perception in self._new_percepts:
                perception.get_perceiver().perceive(perception)
            self._new_percepts = []
            # Decide
            actions = []
            for player in self._players.values()
                actions.append(player.decide(current_time_point))
            self._actions.extend(actions)
            # Execute
            for action in actions:
                self._new_percepts.extend(action.execute())



"""generation_logic.py: Contains the objects and logic for the generations of a community"""

from typing import Dict
from .player_logic import Player, PlayerFactory, PlayerCreationException
from .percept_logic import PerceptionException, PerceptInteraction
import random
import copy
import requests
from flask import current_app
from .action_logic import Interaction, Action, Gossip


class GenerationCreationException(Exception):
    """An exception to raise when there is an error when creating a generation"""

    def __init__(self, message):
        super(GenerationCreationException, self).__init__(message)


class ActionFactory:
    """A class used to generate actions from dictionaries describing the action"""

    @staticmethod
    def generate_action(action_dict: Dict, players: Dict, timepoint: int) -> Action:
        if action_dict['type'] == "interaction":
            return Interaction(timepoint, players[action_dict['donor']],
                               players[action_dict['recipient']], action_dict['cooperated'])
        elif action_dict['type'] == "gossip":
            return Gossip(action_dict['positive'], players[action_dict['about']],
                          players[action_dict['gossiper']],
                          players[action_dict['recipient']], timepoint)


class Generation:
    """A generation of players inside a community"""

    def __init__(self, strategies: Dict[Dict, int], start_time: int, end_time: int,
                 communityID: int, id: int, onlooker_number: int):
        self._players = {}
        self._id = id
        response = requests.request("PUT", current_app.config['AGENTS_URL'] + "generation",
                                 json={"community": communityID, "generation": self._id})
        if response.status_code != 200:
            raise GenerationCreationException("Error when creating generation, not 200 status code")
        if not response.json()['succcess']:
            raise GenerationCreationException("Error when creating generation, bad status")
        id = 0
        for strategy, strategy_count in strategies.items():
            for i in range(strategy_count):
                try:
                    new_player = PlayerFactory.new_player(strategy['name'], strategy['options'], communityID, self._id, id)
                    self._players[new_player.get_id()] = new_player
                    id += 1
                except PlayerCreationException as e:
                    raise e
            id += 1
        self._start_time = start_time
        self._end_time = end_time
        self._communityID = communityID
        self._actions = []
        self._new_percepts = []
        self._onlooker_number = onlooker_number

    def id_to_player(self, id: int) -> Player:
        """Convert the passed id to a player object"""
        return self._players[id]

    def _generate_meeting_percept_and_onlookers(self, timepoint):
        """Generate a donor-recipient pair and the onlookers for the pair
        :return: The first element: a perception that a player is the donor
         and a perception that a player is a recipient
         The second element: a list of onlookers
        """
        ids = copy.deepcopy(self._players)
        donor_id = random.choice(list(ids))
        ids.pop(donor_id, None)
        recipient_id = random.choice(list(ids))
        ids.pop(recipient_id, None)
        onlooker_number = self._onlooker_number
        onlookers = []
        for i in range(onlooker_number):
            if len(ids) > 0:
                onlooker_id = random.choice(ids)
                ids.pop(onlooker_id, None)
                onlookers.append(self._players[onlooker_id])
            else:
                break
        return PerceptInteraction(self._players[donor_id], self._players[recipient_id], timepoint), onlookers

    def simulate(self):
        """Simulate the generation, in each timepoint: generate a donor-recipient pair and onlookers,
        then send perceptions to the agents, then ask them to decide on an action, then execute the action"""
        for current_time_point in range(self._start_time, self._end_time):
            # Generate the donor recipient pair and their onlookers
            donor_recipient_perception, onlookers = self._generate_meeting_percept_and_onlookers(current_time_point)
            # Perceive
            self._new_percepts.append(donor_recipient_perception)
            for perception in self._new_percepts:
                try:
                    perception.perceive(self._communityID, self.get_id())
                except PerceptionException as e:
                    raise e
            self._new_percepts = []
            # Decide
            actions = []
            for player in self._players.values():
                action_dict = player.decide(current_time_point)
                action = ActionFactory.generate_action(action_dict, self._players, current_time_point)
                if type(action) == Interaction:
                    action.set_onlookers(onlookers)
                actions.append(action)
            self._actions.extend(actions)
            # Execute
            for action in actions:
                self._new_percepts.extend(action.execute())

    def get_players(self):
        """
        Get the players in this generation
        :return: Get the players in this generation
        """
        return self._players

    def get_id(self) -> int:
        """
        Get the id of this generation
        :return: The id of this generation
        """
        return self._id

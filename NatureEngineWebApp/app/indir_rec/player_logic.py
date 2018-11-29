"""player_logic.py: Contains the logic to create players, ask players to perceive events,
ask players to decide on an action, and the fitness of a player"""

__author__ = "James King"

import requests
from flask import current_app, jsonify
from typing import Dict, List


class PlayerCreationException(Exception):
    """An exception to raise when there is an error when creating a player"""

    def __init__(self, message):
        super(PlayerCreationException, self).__init__(message)


class DecisionException(Exception):
    """An exception to raise when there is an error when creating a player"""

    def __init__(self, message):
        super(DecisionException, self).__init__(message)


class Player:
    """A class used to represent a player in the environment"""

    def __init__(self, id: int, strategy: str, strategy_options: List, community_id: int, generation_id: int):
        """
        :param id: The id of the player in the agents service
        :param strategy: The name of the strategy of the player
        :param community_id: The id of the community the player belongs to
        :param generation_id:  The id of the generation the player belongs to
        """
        self._id = id
        self._fitness = 0
        self._strategy = strategy
        self._strategy_options = strategy_options
        self._community_id = community_id
        self._generation_id = generation_id

    def get_id(self) -> int:
        """
        Gets the id of the player
        :return: the id of the player
        :rtype: int
        """
        return self._id

    def update_fitness(self, update_value: int):
        """
        Update the fitness of the player by adding the update_value, cannot go below zero
        :param update_value: Add this to the fitness of the player
        :type update_value: int
        """
        self._fitness += update_value
        if self._fitness < 0:
            self._fitness = 0

    def get_fitness(self) -> int:
        """
        Get the current fitness of the player
        :return: the fitness of the player
        :rtype: int
        """
        return self._fitness

    def decide(self, timepoint: int) -> Dict:
        """
        Get the action from the player this cycle step
        :return: The action this player has committed to
        """
        action_data = {"community": self._community_id, "generation": self._generation_id,
                       "player": self._id, "timepoint": timepoint}
        response = requests.request("GET", current_app.config['AGENTS_URL'] + "action", params=action_data)
        if response.status_code != 200:
            raise DecisionException('Error when getting decision from the agents service')
        if not response.json()['success']:
            raise DecisionException('Error when getting decision from the agents service: ' +
                                    response.json()['message'])
        return response.json()['action']

    def get_strategy(self) -> Dict:
        """
        Get the strategy used by the player
        :return: The action this player has committed to
        :rtype: Dict
        """
        return {'name': self._strategy, 'options': self._strategy_options}


class PlayerFactory:
    """A class used to create a new player both in the python application and agents service"""

    @staticmethod
    def new_player(strategy: str, options: List, community_id: int, generation_id: int, player_id: int) -> Player:
        """Creates a new player in both the agents service and python application or raises a PlayerCreationException

        :param strategy: name of the strategy to create
        :type strategy: str
        :param generation_id: the id of the generation this player belongs to
        :type generation_id: int
        :param community_id: id of the community this player is a part of
        :type community_id: str
        :return: A new player instance of the strategy input
        :rtype: Player
        :raises PlayerCreationError: Raised if there is an error in creation of the player"""
        initialise_data = {"strategy": strategy, "options": options, "community": community_id,
                           "generation": generation_id, "player": player_id}
        response = requests.request("PUT", current_app.config['AGENTS_URL'] + "agent", json=initialise_data)
        if response.status_code != 200:
            raise PlayerCreationException('Failed to create player, agent service error')
        if not response.json()['success']:
            raise PlayerCreationException('Failed to create player: ' + response.json()['message'])
        return Player(player_id, strategy, options, community_id, generation_id)

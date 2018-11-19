"""player_logic.py: Contains the logic to create players, ask players to perceive events,
ask players to decide on an action, and the fitness of a player"""

__author__ = "James King"

import requests
from flask import current_app, jsonify
from typing import Dict


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

    def __init__(self, id: int, strategy: str, communityID: int, generationID: int):
        """
        :param id: The id of the player in the agents service
        :param strategy: The name of the strategy of the player
        :param communityID: The id of the community the player belongs to
        :param generationID:  The id of the generation the player belongs to
        """
        self._id = id
        self._fitness = 0
        self._strategy = strategy
        self._communityID = communityID
        self._generationID = generationID

    def get_id(self) -> int:
        """
        Gets the id of the player
        :return: the id of the player
        :rtype: int
        """
        return self._id

    def update_fitness(self, update_value: int):
        """
        Update the fitness of the player by adding the update_value
        :param update_value: Add this to the fitness of the player
        :type update_value: int
        """
        self._fitness += update_value

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
        action_data = {"community": self._communityID, "generation": self._generationID,
                       "agent": self._id, "timepoint": timepoint}
        response = requests.post(current_app.config['AGENTS_URL'] + "decide", json=action_data)
        if response.status_code != 200:
            raise DecisionException('Error when getting decision from the agents service')
        if response.json()['status'] == "Bad":
            raise DecisionException('Error when getting decision from the agents service')
        return response.json()


class PlayerFactory:
    """A class used to create a new player both in the python application and agents service"""

    @staticmethod
    def new_player(strategy: str, communityID: int, generationID: int, playerID: int) -> Player:
        """Creates a new player in both the agents service and python application or raises a PlayerCreationException

        :param strategy: name of the strategy to create
        :type strategy: str
        :param generationID: the id of the generation this player belongs to
        :type generationID: int
        :param communityID: id of the community this player is a part of
        :type communityID: str
        :return: A new player instance of the strategy inpuy
        :rtype: Player
        :raises PlayerCreationError: Raised if there is an error in creation of the player"""
        initialise_data = {"strategy": strategy, "community": communityID,
                           "generation": generationID, "player": playerID}
        response = requests.post(current_app.config['AGENTS_URL'] + "create/new_agent", json=initialise_data)
        if response.status_code != 200:
            raise PlayerCreationException('Failed to create player, agent service error')
        if response.json()['status'] != "Good":
            raise PlayerCreationException('Failed to create player, status is bad')
        return Player(playerID, strategy, communityID, generationID)

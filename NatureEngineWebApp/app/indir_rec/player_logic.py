"""player_logic.py: Contains the logic to create players, ask players to perceive events,
ask players to decide on an action, and the fitness of a player"""

__author__ = "James King"

import requests
from flask import current_app
from abc import ABC
from typing import List
from app.indir_rec.action_logic import Action, Interaction, Gossip
from app.indir_rec.percept_logic import Percept
from app.indir_rec.community_logic import Generation


class PlayerCreationException(Exception):
    """An exception to raise when there is an error when creating a player"""

    def __init__(self, message):
        super(PlayerCreationException, self).__init__(message)


class PerceptionError(Exception):
    """An exception to raise when there is an error sending percepts to a player"""

    def __init__(self, message):
        super(PerceptionError, self).__init__(message)


class DecisionError(Exception):
    """An exception to raise when there is an error when creating a player"""

    def __init__(self, message):
        super(DecisionError, self).__init__(message)


class Player(ABC):
    """The abstract class for a player to be implemented from"""

    def __init__(self, id: int, generation: Generation):
        self._id = id
        self._fitness = 0
        self._generation = generation

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

    @classmethod
    def decide(cls, timepoint: int) -> Action:
        """The player decides on an action to take during """
        raise NotImplementedError

    @classmethod
    def perceive(cls, new_percepts: List[Percept]):
        """The player perceives new events"""
        raise NotImplementedError


class PrologPlayer(Player):
    """A class used to represent a player in the environment"""

    def __init__(self, id: int, strategy: str, generation: Generation):
        """
        :param id: The id of the player in the agents service
        :param strategy: The name of the strategy of the player
        """
        super(PrologPlayer, self).__init__(id, generation)
        self._strategy = strategy

    def perceive(self, new_percepts: List[Percept]):
        """
        Add the percepts to the prolog player
        :param new_percepts: the percepts to send to the prolog player
        """
        percepts = [new_percept.get_percept() for new_percept in new_percepts]
        perception_data = {"perceiverId": self._id, "percepts": percepts}
        response = requests.post(current_app.config['AGENTS_URL'] + "perceive", json=perception_data)
        if response.json()['status'] == "Bad":
            raise PerceptionError('Error when sending perception to the agents service')

    def decide(self, timepoint: int) -> Action:
        """
        Get the action from the player this cycle step
        :return: The action this player has committed to
        """
        action_data = {"actor": self._id, "timepoint": timepoint}
        response = requests.post(current_app.config['AGENTS_URL'] + "decide", json=action_data).json()
        if response['status'] == "Bad":
            raise DecisionError('Error when getting decision from the agents service')
        if response['type'] == "interaction":
            return Interaction(response['timepoint'], self._generation.id_to_player(response['donor']),
                               self._generation.id_to_player(response['recipient']), response['cooperated'])
        elif response['type'] == "gossip":
            return Gossip(response['positive'], self._generation.id_to_player(response['about']),
                          self._generation.id_to_player(response['gossiper']),
                          self._generation.id_to_player(response['recipient']), response['timepoint'])


class PlayerFactory:
    """A class used to create a new player both in the python application and agents service"""

    @staticmethod
    def new_player(strategy: str) -> Player:
        """Creates a new player in both the agents service and python application or raises a PlayerCreationException

        :param strategy: name of the strategy to create
        :type strategy: str
        :return: A new player instance of the strategy inpuy
        :rtype: Player
        :raises PlayerCreationError: Raised if there is an error in creation of the player"""
        initialise_data = {"strategy": strategy}
        response = requests.post(current_app.config['AGENTS_URL'] + "new_agent", json=initialise_data)
        if response.json()['status'] == "Good":
            return PrologPlayer(response.json()['id'], strategy)
        raise PlayerCreationException('Failed to create player')

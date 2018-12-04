
from typing import Dict
from flask import current_app
import requests


class PlayerCreationException(Exception):

    def __init__(self, message):
        super().__init__("Error creating player: " + message)


class DecisionException(Exception):

    def __init__(self, message):
        super().__init__("Error getting decision from player: " + message)


class Player:

    def __init__(self, player_id: int, strategy: Dict, community_id: int, generation_id: int):
        """
        Create a player in the environment and their mind in the agent mind service.
        :param player_id: The player's id
        :type player_id: int
        :param strategy: The player's strategy, including the name, options and description
        :type strategy: Dict
        :param community_id: The id of the community this player belongs to
        :type community_id: int
        :param generation_id: The id of the generation this player belongs to
        :type generation_id: int
        """
        self._player_id: int = player_id
        self._fitness: int = 0
        self._strategy: Dict = strategy
        self._community_id: int = community_id
        self._generation_id: int = generation_id
        try:
            creation_payload: Dict = {"strategy": strategy['name'], "options": strategy['options'],
                                      "community": community_id, "generation": generation_id, "player": player_id}
            creation_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'agent',
                                                 json=creation_payload)
            if creation_response.status_code != 200:
                raise PlayerCreationException("bad status code " + creation_response.status_code)
            if not creation_response.json()['success']:
                raise PlayerCreationException(creation_response.json()['message'])
        except KeyError:
            raise PlayerCreationException("Incorrect strategy keys")

    def get_id(self) -> int:
        """
        Get the id of the player.
        :return: The id of this player
        :rtype: int
        """
        return self._player_id

    def get_fitness(self) -> int:
        """
        Get the fitness of the player.
        :return: The fitness of the player
        :rtype: int
        """
        return self._fitness

    def update_fitness(self, change: int):
        """
        Update the fitness of the player, it cannot go below zero.
        :param change: The change in fitness to be applied
        :return: void
        """
        self._fitness += change
        if self._fitness < 0:
            self._fitness = 0

    def get_strategy(self) -> Dict:
        """
        Get the strategy (name, description and options) of this player
        :return: the strategy of this player
        :rtype: Dict
        """
        return self._strategy

    def decide(self, timepoint: int) -> Dict:
        """
        Get the agents decision on an action to commit to in a certain turn.
        :param timepoint: The timepoint at which the agent is deciding
        :type timepoint: int
        :return: A dictionary representation of the data of the action the player has decided on
        :rtype: Dict
        """
        action_payload: Dict = {"timepoint": timepoint, "community": self._community_id,
                          "generation": self._generation_id, "player": self._player_id}
        action_response = requests.request("GET", current_app.config['AGENTS_URL'] + 'action',
                                           params=action_payload)
        if action_response.status_code != 200:
            raise DecisionException("bad status code " + action_response.status_code)
        if not action_response.json()['success']:
            raise DecisionException(action_response.json()['message'])
        action_representation = action_response.json()['action']
        if action_representation['type'] != "idle" and action_representation['type'] != "gossip" and \
                action_representation['type'] != "action":
            raise DecisionException("Action did not match idle, gossip or action")
        return action_representation

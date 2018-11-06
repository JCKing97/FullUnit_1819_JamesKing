import requests
from flask import current_app
from abc import ABC
from typing import Dict, List

# class Population:
#
#     def __init__(self, players, num_of_gens):
#         self._initial_players = players
#         self._generations = []
#         self._current_time = 0
#         self._num_of_gens = num_of_gens
#
#
# class Generation:
#
#     def __init__(self, players, start_time):
#         self._players = players
#         self._interactions = []
#         self._current_time = start_time
#
#     def get_players(self):
#         return self._players
#
#     def get_interactions(self):
#         return self._interactions


class Event(ABC):
    """An interface for events that can occur during the simulation"""

    @classmethod
    def get_event(cls) -> Dict:
        """Get the details of the event"""
        raise NotImplementedError

    @classmethod
    def event_occurs(cls):
        """Run the event in the agents service and web application"""
        raise NotImplementedError



class PlayerCreationException(Exception):
    """An exception to raise when there is an error when creating a player"""

    def __init__(self, message):
        super(PlayerCreationException, self).__init__(message)


class Player:
    """A class used to represent a player in the environment"""

    def __init__(self, id: int, strategy: str):
        """
        :param id: The id of the player in the agents service
        :param strategy: The name of the strategy of the player
        """
        self._fitness = 0
        self._id = id
        self._strategy = strategy

    def get_id(self) -> int:
        """
        Gets the id of the player
        :return: the id of the player
        :rtype: int
        """
        return self._id

    def get_action(self, other_player) -> str:
        """
        Contact the agent service and get the action a player wants to do as a donor to another player
        :param other_player: The recipient of the interaction
        :return: Either defect or cooperate
        :rtype: str
        """
        donor_recipient_data = {"donorID": self._id, "recipientID": other_player.get_id()}
        response = requests.post(current_app.config['AGENTS_URL'] + "get_action", json=donor_recipient_data)
        return response.json()['action']

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


class PlayerFactory:
    """A class used to create a new player both in the python application and agents service"""

    def new_player(self, strategy: str) -> Player:
        """Creates a new player in both the agents service and python application or raises a PlayerCreationException

        :param strategy: name of the strategy to create
        :type strategy: str
        :return: A new player instance of the strategy inpuy
        :rtype: Player
        :raises PlayerCreationError: Raised if there is an error in creation of the player"""
        initialise_data = {"strategy": strategy}
        response = requests.post(current_app.config['AGENTS_URL'] + "new_agent", json=initialise_data)
        if response.json()['status'] == "Good":
            return Player(response.json()['id'], strategy)
        raise PlayerCreationException('Failed to create player')



class Interaction(Event):
    """An interaction that occurs between a donor-recipient pair, with other players onlooking"""

    def __init__(self, timepoint: int, donor: Player, recipient: Player, onlookers: List[Player]):
        """
        :param timepoint: The timepoint at which this interaction occurred
        :type timepoint: int
        :param donor: The donor of the donor-recipient pair in this interaction
        :type donor: Player
        :param recipient:The recipient of the donor-recipient pair in this interaction
        :type recipient: Player
        :param onlookers: The players who have directly observed this interaction
        :type onlookers: List[Player]
        """
        self._timepoint = timepoint
        self._donor = donor
        self._recipient = recipient
        self._onlookers = onlookers
        self._occurred = False
        self._cooperated = None

    def get_event(self) -> Dict:
        """
        Get a dictionary representation of the event,
         if the event has occurred include whether the donor cooperated or not
        :return: A dictionary representation of the event
        :rtype: Dict
        """
        event_dict = {"donor": self._donor, "recipient": self._recipient,
                      "onlookers": self._onlookers, "timepoint": self._timepoint}
        if self._occurred:
            event_dict['cooperated'] = self._cooperated
        return event_dict

    


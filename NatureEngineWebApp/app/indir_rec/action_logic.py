"""action_logic.py: Contains the logic to execute actions and to get their representation in a dictionary"""

from abc import ABC
from typing import Dict, List
from .percept_logic import Percept, PerceptAction
from .player_logic import Player


class Action(ABC):
    """An interface for events that can occur during the simulation"""

    @classmethod
    def get_action(cls) -> Dict:
        """Get the details of the event"""
        raise NotImplementedError

    @classmethod
    def execute(cls) -> List[Percept]:
        """Execute the action in the environment, generating percepts for the involved players"""
        raise NotImplementedError

    @classmethod
    def set_onlookers(cls, onlookers: List[Player]):
        """Set the onlookers for this action"""
        raise NotImplementedError

    @classmethod
    def get_type(cls) -> str:
        """
        Get the type of the action
        :return: the type of the action as a string
        :rtype: str
        """
        raise NotImplementedError


class Interaction(Action):
    """An interaction that occurs between a donor-recipient pair, with other players onlooking"""

    def __init__(self, timepoint: int, donor: Player, recipient: Player, cooperated: bool):
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
        self._occurred = False
        self._cooperated = cooperated
        self._onlookers = []

    def get_type(self) -> str:
        """
        Get the type of the action
        :return: the type of the action as a string
        :rtype: str
        """
        return "interaction"

    def get_action(self) -> Dict:
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

    def set_onlookers(self, onlookers: List[Player]):
        """
        Set the onlookers for the interaction
        :param onlookers: The onlookers for the interaction
        :return: nothing
        """
        self._onlookers = onlookers

    def execute(self) -> List[Percept]:
        """
        Execute the action, producing a list of percepts from it
        :return: The percepts produces from the action
        :rtype: List[Percept]
        """
        self._occurred = True
        if self._cooperated:
            self._donor.update_fitness(-1)
            self._recipient.update_fitness(2)
        generated_percepts = [PerceptAction(onlooker, self.get_action()) for onlooker in self._onlookers]
        generated_percepts.append(PerceptAction(self._donor, self.get_action()))
        generated_percepts.append(PerceptAction(self._recipient, self.get_action()))
        return generated_percepts


class Gossip(Action):
    """A piece of gossip about a certain player"""

    def __init__(self, positive: bool, about: Player, gossiper: Player, recipient: Player, timepoint: int):
        """
        :param positive: Is the gossip positive?
        :type positive: bool
        :param about: The player that the gossip is about
        :type about: Player
        :param gossiper: The player that spread the gossip
        :type gossiper: Player
        :param timepoint: The timepoint at which this gossip occurs
        :type timepoint: int
        """
        self._positive = positive
        self._about = about
        self._gossiper = gossiper
        self._recipient = recipient
        self._timepoint = timepoint

    def get_action(self) -> Dict:
        """
        Get a dictionary representation of the gossip
        :return: The dictionary representation of the gossip
        :rtype: Dict
        """
        return {"about": self._about, "gossiper": self._gossiper, "recipient": self._recipient,
                "positive": self._positive, "timepoint": self._timepoint}

    def get_type(self) -> str:
        """
        Get the type of the action
        :return: the type of the action as a string
        :rtype: str
        """
        return "gossip"

    def execute(self) -> List[Percept]:
        """Create the percept from thi gossip"""
        return [PerceptAction(self._recipient, self.get_action())]

    def set_onlookers(cls, onlookers: List[Player]):
        """Set the onlookers for this action"""
        raise NotImplementedError

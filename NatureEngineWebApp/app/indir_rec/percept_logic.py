"""percept_logic.py: Contains the logic and data for a percept for an agent to perceive"""

from typing import Dict
from .player_logic import Player
from abc import ABC
from flask import current_app
import requests


class PerceptionException(Exception):
    """An exception to raise when there is an error sending percepts to a player"""

    def __init__(self, message):
        super(PerceptionException, self).__init__(message)


class Percept(ABC):
    """The abstract class for a singular percept for an agent"""

    def __init__(self, perceiver: Player, timepoint: int):
        self._perceiver = perceiver
        self._timepoint = timepoint

    @classmethod
    def get_percept(cls) -> Dict:
        """Get this percept represented in a dictionary format"""
        raise NotImplementedError

    def perceive(self):
        """
        Add the percepts to the prolog player
        """
        perception_data = {"perceiverId": self._perceiver.get_id(),
                           "timepoint": self._timepoint, "percept": self.get_percept()}
        response = requests.post(current_app.config['AGENTS_URL'] + "perceive", json=perception_data)
        if response.status_code != 200:
            raise PerceptionException('Error when sending perception to the agents service')
        if response.json()['status'] == "Bad":
            raise PerceptionException('Error when sending perception to the agents service')


class PerceptAction(Percept):
    """The percept that an action has occurred"""

    def __init__(self, perceiver: Player, timepoint: int, action_dict: Dict):
        super(PerceptAction, self).__init__(perceiver, timepoint)
        self._action_dict = action_dict

    def get_percept(self) -> Dict:
        return {"type": "action", "content": self._action_dict}


class PerceptDonor(Percept):
    """The percept that the perceiver is a donor in a donor-recipient pair this turn"""

    def __init__(self, perceiver: Player, timepoint: int, recipient: Player):
        super(PerceptDonor, self).__init__(perceiver, timepoint)
        self._recipient = recipient

    def get_percept(self) -> Dict:
        """
        Get the percept as represented in a dictionary of the form {perceiverId: id, "type", "donor"}
        :return: The percept represented by a dictionary
        """
        return {"type": "donor", "recipient": self._recipient.get_id()}


class PerceptRecipient(Percept):
    """The percept that the perceiver is a donor-recipient pair this turn"""

    def __init__(self, perceiver: Player, timepoint: int, donor: Player):
        super(PerceptRecipient, self).__init__(perceiver, timepoint)
        self._donor = donor

    def get_percept(self):
        """
        Get the percept as represented in a dictionary of the form {perceiverId: id, "type", "recipient"}
        :return: The percept represented by a dictionary
        """
        return {"type": "recipient", "donor": self._donor.get_id()}

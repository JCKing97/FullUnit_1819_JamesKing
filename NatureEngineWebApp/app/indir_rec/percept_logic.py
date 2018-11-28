"""percept_logic.py: Contains the logic and data for a percept for an agent to perceive"""

from typing import Dict
from abc import ABC
from flask import current_app
import requests


class PerceptionException(Exception):
    """An exception to raise when there is an error sending percepts to a player"""

    def __init__(self, message):
        super(PerceptionException, self).__init__(message)


class Percept(ABC):
    """The abstract class for a singular percept for an agent"""

    def __init__(self, timepoint: int):
        self._timepoint = timepoint

    @classmethod
    def perceive(cls, community_id: int, generation_id: int):
        """
        Add the percepts to the prolog player
        """
        raise NotImplementedError


class PerceptAction(Percept):
    """The percept that an action has occurred"""

    def __init__(self, perceiver: int, timepoint: int, action_dict: Dict):
        super(PerceptAction, self).__init__(timepoint)
        self._action_dict = action_dict
        self._perceiver = perceiver

    def perceive(self, community_id: int, generation_id: int):
        """
        Add the percepts to the prolog player
        """
        url_extension = self._action_dict['type']
        perception_data = None
        if self._action_dict['type'] == "interaction":
            perception_data = {"community": community_id, "generation": generation_id,
                               "perceiver": self._perceiver, "action": self._action_dict['action'],
                               "donor": self._action_dict['donor'], "recipient": self._action_dict['recipient'],
                               "timepoint": self._timepoint}
        elif self._action_dict['type'] == "gossip":
            perception_data = {"community": community_id, "generation": generation_id,
                               "perceiver": self._perceiver, "gossip": self._action_dict['gossip'],
                               "about": self._action_dict['about'], "gossiper": self._action_dict['gossiper'],
                               "timepoint": self._timepoint}
        if perception_data is not None:
            print(current_app.config['AGENTS_URL'] + "percept/action/" + url_extension)
            response = requests.post(current_app.config['AGENTS_URL'] + "percept/action/" + url_extension,
                                     json=perception_data)
            if response.status_code != 200:
                raise PerceptionException('Error when sending perception to the agents service')
            if not response.json()['success']:
                raise PerceptionException('Error when sending perception to the agents service: ' +
                                          response.json()['message'])
        else:
            raise PerceptionException('Error when sending perception to the agents service')


class PerceptInteraction(Percept):
    """The percept that the perceiver is a donor in a donor-recipient pair this turn"""

    def __init__(self, donor: int, recipient: int, timepoint: int,):
        super(PerceptInteraction, self).__init__(timepoint)
        self._recipient = recipient
        self._donor = donor

    def perceive(self, community_id: int, generation_id: int):
        """
        Add the percepts to the prolog player
        """
        perception_data = {"community": community_id, "generation": generation_id,
                           "donor": self._donor, "recipient": self._recipient,
                           "timepoint": self._timepoint}
        response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/interaction', json=perception_data)
        if response.status_code != 200 or not response.json()['success']:
            raise PerceptionException('Error when sending perception to the agents service'+
                                      response.json()['message'])

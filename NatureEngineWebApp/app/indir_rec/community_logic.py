"""community_logic.py: Contains the objects and logic for communities"""

from typing import Dict
import requests
from flask import current_app
from .generation_logic import Generation, GenerationCreationException
from .player_logic import PlayerCreationException


class CommunityCreationException(Exception):
    """An exception to raise when there is an error when creating a community"""

    def __init__(self, message):
        super(CommunityCreationException, self).__init__(message)


class Community:
    """A community that goes through a number of generations"""

    def __init__(self, initial_strategies: Dict[Dict, int], onlooker_number: int = 5, generation_length: int = 10,
                 generation_number: int = 5):
        self._generations = []
        self._current_time = 0
        self._onlooker_number = onlooker_number
        self._generation_length = generation_length
        self._generation_number = generation_number
        response = requests.request("PUT", current_app.config['AGENTS_URL'] + "community")
        if response.status_code != 200:
            raise CommunityCreationException("Failed when attempting to create community, cannot simulate reciprocity")
        self._id = response.json()['id']
        self._initial_strategies = initial_strategies

    def simulate(self):
        """
        Simulate the community
        """
        for i in range(self._generation_number):
            self._generations.append(self._build_generation(i))
            self._generations[i].simulate()

    def _build_generation(self, generation_id: int) -> Generation:
        """
        Build a new generation
        :param generation_id: The id of the generation to create
        :type generation_id: int
        :return: The new generation
        :rtype: Generation
        """
        try:
            if generation_id == 0:
                return Generation(self._initial_strategies, 0, self._generation_length, self._id, 0,
                                  self._onlooker_number)
            else:
                return self._reproduce(generation_id)
        except PlayerCreationException as e:
            raise e
        except GenerationCreationException as e:
            raise e

    def _reproduce(self, generation_id: int) -> Generation:
        """
        Based on reproduction rules using fitness create a new generation based on the last
        :return: The new generation
        :rtype: Generation
        """
        # REPLACE THIS IT IS A STUB
        current_time = len(self._generations)*self._generation_length
        return Generation(self._initial_strategies, current_time,
                          current_time+self._generation_length, self._id, generation_id, self._onlooker_number)

    def get_onlooker_number(self) -> int:
        """
        Get the amount of onlookers for each interaction in this community
        :return: The number of onlookers for each interaction
        """
        return self._onlooker_number

    def get_id(self) -> int:
        """
        Get the id of this community
        :return: The id of this commnunity
        """
        return self._id




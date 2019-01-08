
from typing import Dict, List
from flask import current_app
import requests


class PlayerCreationException(Exception):

    def __init__(self, message):
        super().__init__("Error creating player: " + message)


class DecisionException(Exception):

    def __init__(self, message):
        super().__init__("Error getting decision from player: " + message)


class PerceptionException(Exception):

    def __init__(self, message):
        super().__init__("Error perceiving: " + message)


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
        self._percepts: Dict = {}
        self._non_donor_action_count = 0
        self._social_action_count = 0
        self._idle_action_count = 0
        self._cooperation_count = 0
        self._defection_count = 0
        self._actions: Dict[int] = {}
        self._donor_actions: List[Dict] = []
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

    def get_social_activeness(self) -> int:
        """
        Get a social activeness rating out of 100 for this player
        :return: Social activeness rating out of 100
        :rtype: int
        """
        return int(round(100*(self._social_action_count/self._non_donor_action_count)))

    def get_idleness(self) -> int:
        """
        Get idleness rating out of 100
        :return: Idleness rating out of 100
        :rtype: int
        """
        return int(round(100*(self._idle_action_count/self._non_donor_action_count)))

    def get_cooperation_rate(self) -> int:
        """
        Get the cooperation percentage of this player as a donor out of 100
        :return: Cooperation percentage of player
        :rtype: int
        """
        return int(round(100*(self._cooperation_count/(self._cooperation_count+self._defection_count))))

    def get_actions(self) -> Dict[int]:
        """
        Get the actions of the player with the timepoints as index
        :return: The players actions
        :rtype: Dict[int]
        """
        return self._actions

    def get_donor_actions(self) -> List[Dict]:
        """
        Get the actions this player has committed to as a donor
        :return: The actions this player has committed to as a donor
        :rtype: List[Dict]
        """
        return self._donor_actions

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
        if action_representation['type'] == "gossip":
            action_representation['gossiper'] = self.get_id()
        elif action_representation['type'] == "action":
            action_representation['donor'] = self.get_id()
            self._donor_actions.append(action_representation)
        elif action_representation['type'] == 'idle':
            action_representation['player'] = self.get_id()
        self._actions[action_representation['timepoint']] = action_representation
        return action_representation

    def set_perception(self, perception):
        """
        Set a perception into the player's perception bank, ready to be perceived
        :param perception: The perception to set into the player's perception bank
        """
        if perception['timepoint'] not in self._percepts.keys():
            self._percepts[perception['timepoint']] = [perception]
        else:
            self._percepts[perception['timepoint']].append(perception)

    def perceive(self, timepoint):
        """
        Tell the agent to perceive the percepts set for the previous timepoint from this one
        :param timepoint: The timepoint we are currently at so is one in front of the percepts to perceive
        :return: void
        """
        if timepoint > 0 and timepoint-1 in self._percepts:
            percept_dict = {'percepts': self._percepts[timepoint-1]}
            percept_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/action/group',
                                                json=percept_dict)
            if percept_response.status_code != 200:
                raise PerceptionException("Failed to send percept bad status code: " +
                                          str(percept_response.status_code))
            for success_response in percept_response.json()['success']:
                if not success_response['success']:
                    raise PerceptionException(success_response['success'])

"""generation_logic.py: Module for the functionality involved in creating generations and
managing actions, percepts and players"""

import requests
from flask import current_app
from typing import Dict, List
from .player_logic import Player, PlayerCreationException, DecisionException
import copy
import random


class GenerationCreationException(Exception):
    """Exceptions caused when creating generations"""

    def __init__(self, message):
        super().__init__("Error creating generation: " + message)


class SimulationException(Exception):
    """Exceptions caused when simulating generations"""

    def __init__(self, message):
        super().__init__("Error simulating generation: " + message)


class PlayerNotFoundException(Exception):
    """Exceptions caused when attempting to find players from a list"""

    def __init__(self, message):
        super().__init__(message)


class Generation:
    """A generation encompasses a number of timepoints in which members of the generation perceive percepts and act"""

    def __init__(self, strategies: List[Dict], generation_id: int, community_id: int, start_point: int, end_point: int,
                 num_of_onlookers: int):
        """
        Set up a generation and the players that are part of it in the environment and agent mind service
        :param strategies: A list of strategies (name, description and options) and the amount of them that have been
         selected
        :type strategies: List[Dict]
        :param generation_id: The id of this generation
        :type generation_id: int
        :param community_id: The id of the community that this generation belongs to
        :type community_id: int
        :param start_point: The timepoint at which this generation begins
        :type start_point: int
        :param end_point: The timepoint at which this generation ends
        :type end_point: int
        :param num_of_onlookers: The number of onlookers for each action in this generation
        :type num_of_onlookers: int
        """
        if start_point >= end_point:
            raise GenerationCreationException("start point >= end point")
        self._generation_id: int = generation_id
        self._community_id: int = community_id
        self._start_point: int = start_point
        self._end_point: int = end_point
        self._num_of_onlookers = num_of_onlookers
        self._actions: Dict = {}
        self._percepts: Dict = {}
        creation_response = requests.request("PUT", current_app.config['AGENTS_URL'] + 'generation',
                                             json={"community": community_id, "generation": generation_id})
        if creation_response.status_code != 200:
            raise GenerationCreationException("bad status code: " + creation_response.status_code)
        if not creation_response.json()['success']:
            raise GenerationCreationException(creation_response.json()['message'])
        self._players: List[Player] = []
        self._id_player_map: Dict[int, Player] = {}
        player_id = 0
        for strategy in strategies:
            if strategy['count'] > 0:
                for i in range(strategy['count']):
                    try:
                        player = Player(player_id, strategy['strategy'], self._community_id,
                                        self._generation_id)
                        self._players.append(player)
                        self._id_player_map[player.get_id()] = player
                        player_id += 1
                    except PlayerCreationException as e:
                        raise GenerationCreationException(str(e))

    def get_id(self):
        """
        Get the id of this generation
        :return: The id of this generation
        :rtype: int
        """
        return self._generation_id

    def get_start_point(self):
        """
        Get the start timepoint of this generation
        :return: The start timepoint of this generation
        :rtype: int
        """
        return self._start_point

    def get_end_point(self):
        """
        Get the end timepoint of this generation
        :return: The end timepoint of this generation
        :rtype: int
        """
        return self._end_point

    def get_players(self) -> List[Player]:
        """
        Get the players from this generation
        :return: The players from this generation
        :rtype: List[Player]
        """
        return self._players

    def simulate(self):
        """
        Run the cycle steps: perceive, decide execute between the start and end points of this generation
        :return: void
        """
        for timepoint in range(self._start_point, self._end_point):
            self._perceive(timepoint)
            self._actions[timepoint] = self._decide(timepoint)
            self._percepts[timepoint] = self._execute(timepoint)

    def _perceive(self, timepoint):
        """
        Send percepts produced at the last timepoint to players and the perception of for
        the donor-recipient pair this turn
        :param timepoint: The timepoint we are currently at
        :type timepoint: int
        :return: void
        """
        players = copy.deepcopy(self._players)
        donor: Player = random.choice(players)
        players.remove(donor)
        recipient: Player = random.choice(players)
        interaction_payload = {'donor': donor.get_id(), 'recipient': recipient.get_id(), 'timepoint': timepoint,
                               'community': self._community_id, 'generation': self._generation_id}
        interaction_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/interaction',
                                                json=interaction_payload)
        if interaction_response.status_code != 200:
            raise SimulationException("Failed to create interaction pair bad status code: " +
                                      interaction_response.status_code)
        if not interaction_response.json()['success']:
            raise SimulationException(interaction_response.json()['message'])
        # Send percepts produced from actions
        if timepoint > 0:
            for percept in self._percepts[timepoint-1]:
                if percept['type'] == "action":
                    percept['type'] = "action/interaction"
                elif percept['type'] == "gossip":
                    percept['type'] = "action/gossip"
                percept_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/'
                                                    + percept['type'], json=percept)
                if percept_response.status_code != 200:
                    raise SimulationException("Failed to send percept bad status code: " + str(percept_response.status_code))
                if not percept_response.json()['success']:
                    raise SimulationException(percept_response.json()['message'])

    def _decide(self, timepoint) -> List[Dict]:
        """
        Call all players to decide on an action at the given timepoint, then processing the data to represent an action
        :param timepoint: The timepoint for the players to decide at
        :type timepoint: int
        :return: A list of the data for player actions produced at this time
        :rtype: List[Dict]
        """
        new_actions = []
        for player in self._players:
            try:
                decision = player.decide(timepoint)
            except DecisionException as e:
                raise SimulationException("Failed to get a decision: " + str(e))
            if decision['type'] == "gossip":
                decision['gossiper'] = player.get_id()
                print("decision: " + str(decision))
            elif decision['type'] == "action":
                decision['donor'] = player.get_id()
                print("decision: " + str(decision))
            new_actions.append(decision)
        return new_actions

    def _execute(self, timepoint) -> List[Dict]:
        """
        Execute the actions at this given timepoint, creating percepts where necessary and updating players fitness
        :param timepoint: The timepoint of actions to execute
        :type timepoint: int
        :return: A list of percepts (processed datato send to the agent mind service) produced from the executed actions
        :rtype: List[Dict]
        """
        # Process actions to get percepts and update players
        new_percepts = []
        for action in self._actions[timepoint]:
            # Process gossip action into a percept
            if action['type'] == "gossip":
                gossip_percept = copy.deepcopy(action)
                gossip_percept['type'] = "action/gossip"
                del gossip_percept['value']
                gossip_percept['gossip'] = action['value']
                del gossip_percept['recipient']
                gossip_percept['perceiver'] = action['recipient']
                gossip_percept['community'] = self._community_id
                gossip_percept['generation'] = self._generation_id
                gossip_percept['timepoint'] = timepoint
                new_percepts.append(gossip_percept)
                print("percept : " + str(gossip_percept))
            # Process interaction action into percepts
            elif action['type'] == "action":
                # Alter players fitness when cooperation has been chosen
                if action['value'] == "cooperate":
                    self._id_player_map[action['donor']].update_fitness(-1)
                    self._id_player_map[action['recipient']].update_fitness(2)
                action_percepts = []
                onlookers = self._generate_onlookers(action)
                for onlooker in onlookers:
                    action_percept = copy.deepcopy(action)
                    del action_percept['type']
                    action_percept['type'] = "action/interaction"
                    del action_percept['value']
                    action_percept['action'] = action['value']
                    action_percept['perceiver'] = onlooker.get_id()
                    action_percept['community'] = self._community_id
                    action_percept['generation'] = self._generation_id
                    action_percept['timepoint'] = timepoint
                    action_percepts.append(action_percept)
                    print("percept : " + str(action_percept))
                new_percepts.extend(action_percepts)
        return new_percepts

    def _generate_onlookers(self, action: Dict) -> List[Player]:
        """
        Generate onlookers for an action including the donor and recipient
        :param action: The action to generate onlookers for
        :type action: Dict
        :return: A list of onlookers including the donor and recipient
        :rtype: List[Player]
        """
        players = copy.deepcopy(self._players)
        onlookers = [self._id_player_map[action['donor']], self._id_player_map[action['recipient']]]
        players.remove(self._find_deepcopy_player(action['recipient'], players))
        players.remove(self._find_deepcopy_player(action['donor'], players))
        for i in range(self._num_of_onlookers):
            if len(players) <= 0:
                onlooker = random.choice(players)
                try:
                    onlookers.append(self._id_player_map[onlooker.get_id()])
                except PlayerNotFoundException as e:
                    raise e
                players.remove(onlooker)
        return onlookers

    def _find_deepcopy_player(self, player_id, deep_players) -> Player:
        """
        Find a player in a list given the player list and the player id, for when _id_player_map won't
         work with a deepcopy
        :param player_id: The id of the player we are searching for
        :type player_id: int
        :param deep_players: The players we are searching through to find the id
        :return: The player object from the given list
        :rtype: Player
        """
        for player in deep_players:
            if player_id == player.get_id():
                return player
        raise PlayerNotFoundException("Couldn't find player from ID")


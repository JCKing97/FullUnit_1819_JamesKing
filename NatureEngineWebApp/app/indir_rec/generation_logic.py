"""generation_logic.py: Module for the functionality involved in creating generations and
managing actions, percepts and players"""

import requests
from flask import current_app
from typing import Dict, List
from .player_logic import Player, PlayerCreationException, DecisionException, PerceptionException
from .action_logic import Action, ActionType, GossipAction, InteractionAction
from .observation_logic import Observer
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
                 num_of_onlookers: int, initial_generation: bool, observers: List[Observer]):
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
        self._initial_generation: bool = initial_generation
        self._strategies: List[Dict] = []
        creation_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'generation',
                                             json={"community": community_id, "generation": generation_id})
        if creation_response.status_code != 200:
            raise GenerationCreationException("bad status code: " + str(creation_response.status_code))
        if not creation_response.json()['success']:
            raise GenerationCreationException(creation_response.json()['message'])
        self._players: List[Player] = []
        self._id_player_map: Dict[int, Player] = {}
        player_id = 0
        self._observers = observers
        if initial_generation:
            self._strategies = strategies
            try:
                for strategy in strategies:
                    if strategy['count'] > 0:
                        for i in range(strategy['count']):
                            try:
                                player = Player(player_id, strategy['strategy'], self._community_id,
                                                self._generation_id, self._observers)
                                self._players.append(player)
                                self._id_player_map[player.id] = player
                                player_id += 1
                            except PlayerCreationException as e:
                                raise GenerationCreationException(str(e))
            except KeyError:
                raise GenerationCreationException("Incorrect strategies dictionary keys")
        else:
            for strategy in strategies:
                found_strategy = False
                for self_strategy in self._strategies:
                    if self_strategy['strategy'] == strategy:
                        found_strategy = True
                        self_strategy['count'] += 1
                if not found_strategy:
                    self._strategies.append({'strategy': strategy, 'count': 1})
                try:
                    player = Player(player_id, strategy, self._community_id, self._generation_id, self._observers)
                    self._players.append(player)
                    self._id_player_map[player.id] = player
                    player_id += 1
                except PlayerCreationException as e:
                    raise GenerationCreationException(str(e))

    @property
    def id(self):
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

    def get_strategy_count(self) -> List[Dict]:
        """
        Get the count of each strategy in the generation
        :return: the count of each strategy in the generation
        :rtype: List[Dict]
        """
        return self._strategies

    def simulate(self):
        """
        Run the cycle steps: perceive, decide, execute between the start and end points of this generation
        :return: void
        """
        for timepoint in range(self._start_point, self._end_point):
            try:
                self._set_and_send_donor_recipient_pair(timepoint)
            except SimulationException as e:
                raise e
            for player in self._players:
                try:
                    player.perceive(timepoint)
                except PerceptionException as e:
                    raise SimulationException("Error in player perception: " + str(e))
                try:
                    decision: Action = player.decide(timepoint)
                    self._execute(decision, timepoint)
                except DecisionException as e:
                    raise SimulationException("Error in player decision: " + str(e))

    def _set_and_send_donor_recipient_pair(self, timepoint: int):
        """
        Decides on a donor-recipient pair for this timepoint and sends the percept to the agents service
        :param timepoint: The timepoint to set the pair for
        :type timepoint: int
        :return: void
        """
        players = copy.deepcopy(self._players)
        donor: Player = random.choice(players)
        players.remove(donor)
        recipient: Player = random.choice(players)
        interaction_payload = {'donor': donor.id, 'recipient': recipient.id, 'timepoint': timepoint,
                               'community': self._community_id, 'generation': self._generation_id}
        interaction_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/interaction',
                                                json=interaction_payload)
        if interaction_response.status_code != 200:
            raise SimulationException("Failed to create interaction pair bad status code: " +
                                      interaction_response.status_code)
        if not interaction_response.json()['success']:
            raise SimulationException(interaction_response.json()['message'])

    def _execute(self, action: Action, timepoint: int):
        """
        Execute the actions at this given timepoint, setting percepts for the players where necessary
        and updating players fitness
        :param action: The action to process
        :type action: Dict
        """
        # Process gossip action into a percept
        if action.type is ActionType.GOSSIP:
            gossip_action: GossipAction = action
            gossip_percept = {'type': gossip_action.type.value['percept_link'], 'gossip': gossip_action.gossip.value,
                              'perceiver': gossip_action.recipient, 'community': self._community_id,
                              'generation': self._generation_id, 'timepoint': timepoint}
            self._id_player_map[gossip_percept['perceiver']].set_perception(gossip_percept)
        elif action.type is ActionType.INTERACTION:
            # Alter players fitness when cooperation has been chosen
            interaction_action: InteractionAction = action
            self._id_player_map[interaction_action.donor].update_fitness(interaction_action.action.value['donor_cost'])
            self._id_player_map[interaction_action.recipient].update_fitness(interaction_action.action.value['recipient_gain'])
            onlookers = self._generate_onlookers(interaction_action)
            for onlooker in onlookers:
                action_percept = {'type': interaction_action.type.value['percept_link'],
                                  'action': interaction_action.action.value['string'], 'perceiver': onlooker.id,
                                  'community': self._community_id, 'generation': self._generation_id,
                                  'timepoint': timepoint}
                self._id_player_map[action_percept['perceiver']].set_perception(action_percept)

    def _generate_onlookers(self, action: InteractionAction) -> List[Player]:
        """
        Generate onlookers for an action including the donor and recipient
        :param action: The action to generate onlookers for
        :type action: Dict
        :return: A list of onlookers including the donor and recipient
        :rtype: List[Player]
        """
        players = copy.deepcopy(self._players)
        onlookers = [self._id_player_map[action.donor], self._id_player_map[action.recipient]]
        players.remove(self._find_deepcopy_player(action.recipient, players))
        players.remove(self._find_deepcopy_player(action.donor, players))
        for i in range(self._num_of_onlookers):
            if len(players) >= 0:
                onlooker = random.choice(players)
                try:
                    onlookers.append(self._id_player_map[onlooker.id])
                except PlayerNotFoundException as e:
                    raise e
                players.remove(onlooker)
            else:
                break
        return onlookers

    @staticmethod
    def _find_deepcopy_player(player_id, deep_players) -> Player:
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
            if player_id == player.id:
                return player
        raise PlayerNotFoundException("Couldn't find player from ID")


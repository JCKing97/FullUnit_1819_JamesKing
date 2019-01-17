
from typing import Dict, List
from flask import current_app
import requests
from .action_logic import Action, InteractionAction, GossipAction, IdleAction, GossipContent, InteractionContent


class PlayerCreationException(Exception):

    def __init__(self, message):
        super().__init__("Error creating player: " + message)


class DecisionException(Exception):

    def __init__(self, message):
        super().__init__("Error getting decision from player: " + message)


class PerceptionException(Exception):

    def __init__(self, message):
        super().__init__("Error perceiving: " + message)


class PlayerState:

    def __init__(self, generation: int, player: int, observers: List = None):
        self._generation = generation
        self._player = player
        self._new_action: Action = None
        self._fitness_update = 0
        self._observers: List = observers if observers is not None else []

    def attach(self, observer):
        self._observers.append(observer)

    def detach(self, observer):
        self._observers.remove(observer)

    def _notify(self):
        for observer in self._observers:
            observer.update(self)

    @property
    def generation(self) -> int:
        return self._generation

    @property
    def player(self) -> int:
        return self._player

    @property
    def new_action(self) -> Action:
        return self._new_action

    @new_action.setter
    def new_action(self, action: Action) -> None:
        self._new_action = action
        self._notify()
        self._new_action = None

    @property
    def fitness_update(self) -> int:
        return self._fitness_update

    @fitness_update.setter
    def fitness_update(self, fitness: int):
        self._fitness_update = fitness
        self._notify()
        self._fitness_update = 0


class Player:

    def __init__(self, player_id: int, strategy: Dict, community_id: int, generation_id: int,
                 observers: List = None):
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
        self.player_state = PlayerState(generation_id, player_id, observers)
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

    @property
    def id(self) -> int:
        """
        Get the id of the player.
        :return: The id of this player
        :rtype: int
        """
        return self._player_id

    @property
    def fitness(self) -> int:
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
            change = 0
        self.player_state.fitness_update = change

    @property
    def strategy(self) -> Dict:
        """
        Get the strategy (name, description and options) of this player
        :return: the strategy of this player
        :rtype: Dict
        """
        return self._strategy

    def decide(self, timepoint: int) -> Action:
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
        if action_representation['type'] == "gossip":
            gossip: GossipContent = GossipContent.POSITIVE if action_representation['gossip'] == 'positive'\
                else GossipContent.NEGATIVE
            action: GossipAction = GossipAction(timepoint, self.id, self._generation_id, action_representation['about'],
                                                action_representation['recipient'], gossip)
        elif action_representation['type'] == "action":
            action_content: InteractionContent = InteractionContent.COOPERATE if \
                action_representation['value'] == 'cooperate' else InteractionContent.DEFECT
            action: InteractionAction = InteractionAction(timepoint, self.id, self._generation_id, action_representation['recipient'],
                                                          action_content)
        elif action_representation['type'] == 'idle':
            action: IdleAction = IdleAction(timepoint, self.id, self._generation_id)
        else:
            raise DecisionException("Action did not match idle, gossip or action")
        self.player_state.new_action = action
        return action

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
            print(percept_dict)
            percept_response = requests.request("POST", current_app.config['AGENTS_URL'] + 'percept/action/group',
                                                json=percept_dict)
            if percept_response.status_code != 200:
                raise PerceptionException("Failed to send percept bad status code: " +
                                          str(percept_response.status_code))
            for success_response in percept_response.json()['success']:
                if not success_response['success']:
                    raise PerceptionException(success_response['success'])

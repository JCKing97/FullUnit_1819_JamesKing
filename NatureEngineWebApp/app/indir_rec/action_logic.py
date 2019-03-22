"""action_logic.py: contains the logic related to actions that players decide on and generations execute"""

__author__ = "James King"

from enum import Enum
from abc import ABC, abstractmethod
from typing import List, NoReturn


class ActionType(Enum):
    """All the possible types of actions a player can commit to, with some values for use when sending percepts"""
    INTERACTION = {'type': 'interaction', 'percept_link': 'action/interaction'}
    GOSSIP = {'type': 'gossip', 'percept_link': 'action/gossip'}
    IDLE = {'type': 'idle'}


class InteractionContent(Enum):
    """The two possible actions a player can commit in an interaction, the dictionary value
     contains the payoff matrix details and the string representaiton"""
    COOPERATE = {'donor_cost': -1, 'recipient_gain':  2, 'string': 'cooperate'}
    DEFECT = {'donor_cost': 0, 'recipient_gain':  0, 'string': 'defect'}


class GossipContent(Enum):
    """The possible communicative content of a gossip action as according to the Simple Agent Gossip Lanaguage (SAGL),
     the value is the conversion to a string"""
    POSITIVE = 'positive'
    NEGATIVE = 'negative'


class Action(ABC):
    """A abstract class base for the representation of any action produced by a player"""

    def __init__(self, timepoint: int, actor: int, generation: int, reason: str):
        """
        Create the base data for any possible action
        :param timepoint: The timepoint at which the action was committed to
        :type timepoint: int
        :param actor: The id for the agent who committed to the action
        :type actor: int
        :param generation: The id for the generation the agent is a member of
        :type generation: int
        :param reason: An agent gives a reason for any action they commit to
        :type reason: str
        """
        self._timepoint: int = timepoint
        self._actor: int = actor
        self._generation = generation
        self._reason: str = reason

    @property
    @abstractmethod
    def type(self) -> ActionType:
        """
        Return the type of the action as and ActionType enum
        :return: The ActionType of this action
        """
        raise NotImplementedError

    @property
    def generation(self) -> int:
        """
        Return the generation id this action was committed to in
        :return: the generation id
        :rtype: int
        """
        return self._generation

    @property
    def timepoint(self) -> int:
        """
        Return the timepoint this action was committed to at
        :return: the timepoint
        :rtype: int
        """
        return self._timepoint

    @property
    def actor(self) -> int:
        """
        Return the actor id that committed this action
        :return: the actor id
        :rtype: int
        """
        return self._actor

    @property
    def reason(self) -> str:
        """
        Return the reason that the actor committed to this action
        :return: the reason behind the action
        :rtype: str
        """
        return self._reason


class IdleAction(Action):
    """An idle action committed by a player in which the player is inactive"""

    def __init__(self, timepoint: int, actor: int, generation: int, reason: str):
        """
        Create the idle action and it's relevant data, including the call to the super with the relevant data
        :param timepoint: The timepoint at which the action was committed to
        :type timepoint: int
        :param actor: The id for the agent who committed to the action
        :type actor: int
        :param generation: The id for the generation the agent is a member of
        :type generation: int
        :param reason: An agent gives a reason for any action they commit to
        :type reason: str
        """
        super(IdleAction, self).__init__(timepoint, actor, generation, reason)

    @property
    def type(self) -> ActionType:
        """
        Return that the type of the action is IDLE
        :return: ActionType.IDLE
        :rtype: ActionType
        """
        return ActionType.IDLE


class GossipAction(Action):
    """An action where the actor has gossipped to the recipient either positive or negative gossip about the about
    agent, fits the SAGL"""

    def __init__(self, timepoint: int, gossiper: int, generation: int, reason: str, about: int, recipient: int,
                 gossip: GossipContent):
        """
        Create the relevant data for a gossip action, including the call to the super abstract class
        :param timepoint: The timepoint at which the action was committed to
        :type timepoint: int
        :param gossiper: The id for the agent who committed to the action
        :type gossiper: int
        :param generation: The id for the generation the agent is a member of
        :type generation: int
        :param reason: An agent gives a reason for any action they commit to
        :type reason: str
        :param about: The id of the agent the gossip is about
        :type about: int
        :param recipient: The id of the agent who receives the gossip
        :type recipient: int
        :param gossip: The content of the gossip as according to the SAGL (either positive or negative)
        :type gossip: GossipContent
        """
        super(GossipAction, self).__init__(timepoint, gossiper, generation, reason)
        self._gossiper = gossiper
        self._about: int = about
        self._recipient: int = recipient
        self._gossip: GossipContent = gossip

    @property
    def type(self) -> ActionType:
        """
        Return the type of the action (gossip)
        :return: ActionType.GOSSIP
        :rtype: ActionType
        """
        return ActionType.GOSSIP

    @property
    def gossiper(self) -> int:
        """
        Return the id of the agent who gossiped the information
        :return: the gossiper's id
        :rtype: int
        """
        return self._gossiper

    @property
    def about(self) -> int:
        """
        Return the id of the agent the gossip is about
        :return: the about id
        :rtype: int
        """
        return self._about

    @property
    def recipient(self) -> int:
        """
        Return the id of the recipient who perceives the gossip
        :return: The recipient id
        :rtype: int
        """
        return self._recipient

    @property
    def gossip(self) -> GossipContent:
        """
        The content of the gossip (enum of either positive of negative
        :return: the gossip content
        :rtype: GossipContent
        """
        return self._gossip


class InteractionAction(Action):
    """An action committed to by a donor of a donor-recipient interaction pair"""

    def __init__(self, timepoint: int, donor: int, generation: int, reason: str, recipient: int,
                 action: InteractionContent, onlookers: List[int] = None):
        """
        Create the relevant fields and fill with the passed content to set up the data from the produced action
        :param timepoint: The timepoint at which the action was committed to
        :type timepoint: int
        :param donor: The id for the agent who committed to the action
        :type donor: int
        :param generation: The id for the generation the agent is a member of
        :type generation: int
        :param reason: An agent gives a reason for any action they commit to
        :type reason: str
        :param recipient: The id of recipient of the action
        :type recipient: int
        :param action: The action committed to by the actor (either cooperate or defect)
        :type action: InteractionContent
        :param onlookers: (optional) A list of the ids of of the players who are onlookers for the action
        :type onlookers: List[int]
        """
        super(InteractionAction, self).__init__(timepoint, donor, generation, reason)
        self._donor: int = donor
        self._recipient: int = recipient
        self._action: InteractionContent = action
        self._onlookers: List[int] = onlookers if onlookers is not None else []

    @property
    def type(self) -> ActionType:
        """
        Return the type of the action (INTERACTION)
        :return: ActionType.INTERACTION
        :rtype: ActionType
        """
        return ActionType.INTERACTION

    @property
    def donor(self) -> int:
        """
        Return the id of the donor who committed to the action
        :return: The donor id
        :rtype: int
        """
        return self._donor

    @property
    def recipient(self) -> int:
        """
        Return the id of the agent who was on the receiving end of the action
        :return: The recipient id
        :rtype: int
        """
        return self._recipient

    @property
    def action(self) -> InteractionContent:
        """
        The action committed to either cooperate or defect
        :return: The action
        :rtype: InteractionContent
        """
        return self._action

    @property
    def onlookers(self) -> List[int]:
        """
        The list of the ids of the onlookers who viewed the interaction
        :return: Onlooker id list
        :rtype: List[int]
        """
        return self._onlookers

    @onlookers.setter
    def onlookers(self, onlookers: List[int]) -> NoReturn:
        """
        Set the ids of the agents who viewed the action (were onlookers)
        :param onlookers: A list of the ids of agents who viewed the action
        :type onlookers: List[int]
        :return: NoReturn
        :rtype: NoReturn
        """
        self._onlookers = onlookers

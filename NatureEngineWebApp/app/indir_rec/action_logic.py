"""action_logic.py: contains the logic related to actions that players decide on and generations execute"""

__author__ = "James King"

from enum import Enum
from abc import ABC, abstractmethod
from typing import List


class ActionType(Enum):
    INTERACTION = {'type': 'interaction', 'percept_link': 'action/interaction'}
    GOSSIP = {'type': 'gossip', 'percept_link': 'action/gossip'}
    IDLE = {'type': 'idle'}


class InteractionContent(Enum):
    COOPERATE = {'donor_cost': -1, 'recipient_gain':  2}
    DEFECT = {'donor_cost': 0, 'recipient_gain':  0}


class GossipContent(Enum):
    POSITIVE = 'positive'
    NEGATIVE = 'negative'


class Action(ABC):

    def __init__(self, timepoint: int, actor: int, generation: int):
        self._timepoint: int = timepoint
        self._actor: int = actor
        self._generation = generation

    @property
    @abstractmethod
    def type(self) -> ActionType:
        raise NotImplementedError

    @property
    def generation(self):
        return self._generation

    @property
    def timepoint(self) -> int:
        return self._timepoint

    @property
    def actor(self) -> int:
        return self._actor


class IdleAction(Action):

    def __init__(self, timepoint: int, actor: int, generation: int):
        super(IdleAction, self).__init__(timepoint, actor, generation)

    @property
    def type(self) -> ActionType:
        return ActionType.IDLE


class GossipAction(Action):

    def __init__(self, timepoint: int, gossiper: int, generation: int, about: int, recipient: int, gossip: GossipContent):
        super(GossipAction, self).__init__(timepoint, gossiper, generation)
        self._gossiper = gossiper
        self._about: int = about
        self._recipient: int = recipient
        self._gossip: int = gossip

    @property
    def type(self) -> ActionType:
        return ActionType.GOSSIP

    @property
    def gossiper(self) -> int:
        return self._gossiper

    @property
    def about(self) -> int:
        return self._about

    @property
    def recipient(self) -> int:
        return self._recipient

    @property
    def gossip(self) -> int:
        return self._gossip


class InteractionAction(Action):

    def __init__(self, timepoint: int, donor: int, generation: int, recipient: int, action: InteractionContent,
                 onlookers: List[int] = None):
        super(InteractionAction, self).__init__(timepoint, donor, generation)
        self._donor: int = donor
        self._recipient: int = recipient
        self._action: InteractionContent = action
        self._onlookers: List[int] = onlookers if onlookers is not None else []

    @property
    def type(self) -> ActionType:
        return ActionType.INTERACTION

    @property
    def donor(self) -> int:
        return self._donor

    @property
    def recipient(self) -> int:
        return self._recipient

    @property
    def action(self) -> InteractionContent:
        return self._action

    @property
    def onlookers(self) -> List[int]:
        return self._onlookers

    @onlookers.setter
    def onlookers(self, onlookers: List[int]):
        self._onlookers = onlookers

"""percept_logic.py: Contains the logic and data for a percept for an agent to perceive"""

from typing import Dict
from app.indir_rec.player_logic import Player
from app.indir_rec.action_logic import Action
from abc import ABC


class Percept(ABC):
    """The abstract class for a singular percept for an agent"""

    def __init__(self, perceiver: Player):
        self._perceiver = perceiver

    def get_perceiver(self) -> Player:
        return self._perceiver

    @classmethod
    def get_percept(cls) -> Dict:
        raise NotImplementedError



class PerceptAction(Percept):

    def __init__(self, perceiver: Player, action: Action):
        super(PerceptAction, self).__init__(perceiver)
        self._action = action

    def get_percept(self) -> Dict:
        return {"perceiverId": self._perceiver.get_id(), "type": "action", "content": self._action.get_action()}


class PerceptDonor(Percept):

    def __init__(self, perceiver: Player):
        super(PerceptDonor, self).__init__(perceiver)

    def get_percept(self):
        return {"perceiverId": self._perceiver.get_id(), "type": "donor"}


class PerceptRecipient(Percept):

    def __init__(self, perceiver: Player):
        super(PerceptRecipient, self).__init__(perceiver)

    def get_percept(self):
        return {"perceiverId": self._perceiver.get_id(), "type": "recipient"}
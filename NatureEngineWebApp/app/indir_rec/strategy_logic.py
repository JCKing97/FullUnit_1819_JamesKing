"""strategy_logic.py: Contains the logic surrounding the assigning of strategies to players"""

from typing import List


class Strategy:

    def __init__(self, donor_strategy: str, non_donor_strategy: str, trust_model: str, options: List):
        self._donor_strategy: str = donor_strategy
        self._non_donor_strategy: str = non_donor_strategy
        self._trust_model: str = trust_model
        self._options: List = options

    @property
    def donor_strategy(self) -> str:
        return self._donor_strategy

    @property
    def trust_model(self) -> str:
        return self._trust_model

    @property
    def non_donor_strategy(self) -> str:
        return self._non_donor_strategy

    @property
    def options(self) -> List:
        return self._options

    def __str__(self):
        to_string = "Strategy: " + self._donor_strategy + ", Strategy Component: " + self._non_donor_strategy +\
                    ", Trust Model: " + self._trust_model + ", Options:"
        for option in self.options:
            to_string += " " + str(option) + ","
        return to_string

    def __eq__(self, other):
        return isinstance(other, Strategy) and self.donor_strategy == other.donor_strategy and \
               self.non_donor_strategy == other.non_donor_strategy and self.trust_model == other.trust_model and\
               self.options == other.options

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        hash_string = self.donor_strategy + self.non_donor_strategy + self.trust_model
        for option in self._options:
            hash_string += str(option)
        return hash_string.__hash__()

"""strategy_logic.py: Contains the logic surrounding the assigning of strategies to players"""

from typing import List


class Strategy:

    def __init__(self, name: str, options: List):
        self._name: str = name
        self._options: List = options

    @property
    def name(self) -> str:
        return self._name

    @property
    def options(self) -> List:
        return self._options

    def __str__(self):
        to_string = self.name + ", options:"
        for option in self.options:
            to_string += " " + str(option) + ","
        return to_string

    def __eq__(self, other):
        return isinstance(other, Strategy) and self._name == other.name and self._options == other.options

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        hash_string = self.name
        for option in self._options:
            hash_string += str(option)
        return hash_string.__hash__()

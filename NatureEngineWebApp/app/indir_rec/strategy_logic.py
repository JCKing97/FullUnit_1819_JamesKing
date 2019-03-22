"""strategy_logic.py: Contains the logic surrounding the assigning of strategies to players"""

from typing import List, Any, Dict


class Strategy:
    """A players strategy"""

    def __init__(self, donor_strategy: str, non_donor_strategy: str, trust_model: str, options: List[Any]):
        """
        Set up the strategy with the passed parameters
        :param donor_strategy: The strategy the player uses when they are a donor
        :type donor_strategy: str
        :param non_donor_strategy: The strategy the player uses when they are not a donor
        :type non_donor_strategy: str
        :param trust_model: The trust model the player uses to interpret events it perceives
        :type trust_model: str
        :param options: The options added to this strategy to augment it's features
        :type options: List[Any]
        """
        self._donor_strategy: str = donor_strategy
        self._non_donor_strategy: str = non_donor_strategy
        self._trust_model: str = trust_model
        self._options: List[Any] = options

    @property
    def donor_strategy(self) -> str:
        """
        Get the part of the strategy used when the agent is a donor
        :return: the strategy when the agent is a donor
        :rtype: str
        """
        return self._donor_strategy

    @property
    def trust_model(self) -> str:
        """
        Get the trust model the agent uses to interpret percepts
        :return: The trust model
        :rtype: str
        """
        return self._trust_model

    @property
    def non_donor_strategy(self) -> str:
        """
        Get the strategy the agent uses when it is not a donor
        :return: the strategy for when the agent is not a donor
        :rtype: str
        """
        return self._non_donor_strategy

    @property
    def options(self) -> List[Any]:
        """
        The options added to the strategy to augment it's features
        :return: The options for the strategy
        :rtype: List[Any]
        """
        return self._options

    def to_dict(self) -> Dict:
        return {'donor_strat': self.donor_strategy, 'non_donor_strat': self.non_donor_strategy,
                'trust_model': self.trust_model, 'options': self.options}

    def __str__(self):
        """Return a string representation of the strategy"""
        to_string = "Strategy: " + self._donor_strategy + ", Strategy Component: " + self._non_donor_strategy +\
                    ", Trust Model: " + self._trust_model + ", Options:"
        for option in self.options:
            to_string += " " + str(option) + ","
        return to_string

    def __eq__(self, other):
        """Check equality to other strategies"""
        return isinstance(other, Strategy) and self.donor_strategy == other.donor_strategy and \
               self.non_donor_strategy == other.non_donor_strategy and self.trust_model == other.trust_model and\
               self.options == other.options

    def __ne__(self, other):
        """Check not equality to other strategies"""
        return not self.__eq__(other)

    def __hash__(self):
        """Get the hash of the strategy"""
        hash_string = self.donor_strategy + self.non_donor_strategy + self.trust_model
        for option in self._options:
            hash_string += str(option)
        return hash_string.__hash__()

"""interface_logic.py: An interface for other software the wishes to run games of indirect reciprocity"""

__author__ = "James King"

from .results_logic import Results
from .community_logic import Community
from typing import List, Dict


class ReputationGame:

    def __init__(self, initial_strategies: List[Dict], num_of_onlookers: int = 5, num_of_generations: int = 10,
                 length_of_generations: int = 30, mutation_chance: float = 0):
        self._initial_strategies = initial_strategies
        self._num_of_onlookers = num_of_onlookers
        self._num_of_generations = num_of_generations
        self._length_of_generations = length_of_generations
        self._mutation_chance = mutation_chance

    @property
    def initial_strategies(self) -> List[Dict]:
        return self._initial_strategies

    @initial_strategies.setter
    def initial_strategies(self, initial_strategies: List[Dict]) -> None:
        self._initial_strategies = initial_strategies

    @property
    def num_of_onlookers(self) -> int:
        return self._num_of_onlookers

    @num_of_onlookers.setter
    def num_of_onlookers(self, num_of_onlookers: int) -> None:
        self._num_of_onlookers = num_of_onlookers

    @property
    def num_of_generations(self) -> int:
        return self._num_of_generations

    @num_of_generations.setter
    def num_of_generations(self, num_of_generations: int) -> None:
        self._num_of_generations = num_of_generations

    @property
    def length_of_generations(self) -> int:
        return self._length_of_generations

    @length_of_generations.setter
    def length_of_generations(self, length_of_generations: int) -> None:
        self._length_of_generations = length_of_generations

    @property
    def mutation_chance(self) -> float:
        return self._mutation_chance

    @mutation_chance.setter
    def mutation_chance(self, mutation_chance: int) -> None:
        self._mutation_chance = mutation_chance

    def run(self) -> Results:
        community = Community(self._initial_strategies, num_of_onlookers=self._num_of_onlookers,
                              num_of_generations=self._num_of_generations,
                              length_of_generations=self._length_of_generations,
                              mutation_chance=self._mutation_chance)
        results = Results(community)
        community.extend_observers(results.observers)
        community.simulate()
        return results


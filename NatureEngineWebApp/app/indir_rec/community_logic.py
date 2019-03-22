"""community_logic.py: the module for functionality surrounding communities: reproduction,
simulation of a whole tournament,setup of a tournament etc."""

import requests
from typing import List, Dict, NoReturn
from .generation_logic import Generation
import random
from .observation_logic import Observer
from .player_logic import Player
from .indir_rec_config import Config
from .strategy_logic import Strategy


class CommunityCreationException(Exception):
    """Exception generated when failed to create a community"""

    def __init__(self, message):
        super().__init__("Error creating community: " + message)


class Community:
    """A community encompasses a number of generations, a first generation is selected on construction, the next
    generations are then created using a reproduction algorithm"""

    def __init__(self, strategies: Dict[Strategy, int], num_of_onlookers: int = 5, num_of_generations: int = 10,
                 length_of_generations: int = 30, mutation_chance: float = 0, observers: List[Observer] = None):
        """
        Set the parameters for the community and the initial set of players to simulate the community with
        :param strategies: The initial set of players to simulate the community
        :type strategies: Dict[Strategy, int]
        :param num_of_onlookers: The number of onlookers for each interaction
        :type num_of_onlookers: int
        :param num_of_generations: The number of generations the simulation will run
        :type num_of_generations: int
        :param length_of_generations: The number of rounds each generation will run for
        :type length_of_generations: int
        """
        # Create the community in the
        community_response = requests.request("POST", Config.AGENTS_URL + 'community')
        self._community_id = community_response.json()['id']
        if community_response.status_code != 200:
            raise CommunityCreationException("Failed to create community in agents service")
        # Ensure the set parameters match the correct conditions, or raise creation exception
        if num_of_onlookers <= 0:
            raise CommunityCreationException("number of onlookers <= 0")
        if length_of_generations <= 5:
            raise CommunityCreationException("length of generations <= 5")
        if num_of_generations <= 2:
            raise CommunityCreationException("number of generations <= 2")
        if mutation_chance > 1 or mutation_chance < 0:
            raise CommunityCreationException("mutation chance should be a probability between 0 and 1")
        self._mutation_chance: float = mutation_chance
        self._num_of_onlookers: int = num_of_onlookers
        self._num_of_generations: int = num_of_generations
        self._length_of_generations: int = length_of_generations
        self._first_strategies: Dict[Strategy, int] = strategies
        self._generations: List[Generation] = []
        self._current_time: int = 0
        self._generation_size: int = 0
        # Count the size of each generation in terms of number of players
        for _, count in strategies.items():
            self._generation_size += count
        self._strategy_count_by_generation: List[Dict[Strategy, int]] = []
        self._observers: List[Observer] = observers if observers is not None else []

    def get_id(self) -> int:
        """
        Get the id of the community
        :return: The id on the community
        :rtype: int
        """
        return self._community_id

    def get_num_of_onlookers(self) -> int:
        """
        Get the number of onlookers for each interaction in this community
        :return: The number of onlooker for each interaction
        :rtype: int
        """
        return self._num_of_onlookers

    def get_length_of_generations(self) -> int:
        """
        Get the number of rounds a generation runs for in this community
        :return: The number of rounds a generation runs for
        :rtype: int
        """
        return self._length_of_generations

    def get_generations(self) -> List[Generation]:
        """
        Get a list of the generations that this community encompasses
        :return: A list of the generations that belong to this community
        :rtype: List[Generation]
        """
        return self._generations

    def get_strategy_count_by_generation(self) -> List[Dict[Strategy, int]]:
        """
        Get the count of each strategy by generation
        :return: The strategy count for each generation
        :rtype: List[List[Dict]]]
        """
        return self._strategy_count_by_generation

    def extend_observers(self, observers: List[Observer]) -> NoReturn:
        """
        Extend the current observers of the community with the list provided
        :param observers: The observers to add to the observers of the community
        :return: NoReturn
        :rtype: NoReturn
        """
        self._observers.extend(observers)

    def simulate(self) -> NoReturn:
        """
        Simulate the community, building an initial generation simulating that generation and then for the specified
        number of generations running the reproduction mechanism and simulating the next generation
        :return: NoReturn
        :rtype: NoReturn
        """
        # For the number of generations specified at the creation
        for i in range(self._num_of_generations):
            # Attach observers that record the statistics on the game
            for observer in self._observers:
                observer.add_generation(i)
            # Create the new generation (the first from the initial set of players, the rest from the reproduciton
            # mechanism)
            generation = self._build_generation(i)
            generation.simulate()
            self._current_time += self._length_of_generations
            self._strategy_count_by_generation.append(generation.get_strategy_count())
            self._generations.append(generation)

    def _build_generation(self, gen_id: int) -> Generation:
        """
        Build a generation, if is is the first generation use the initial strategies, else reproduce from the old
         generation
        :return: The next generation to simulate
        :rtype: Generation
        """
        print("New generation: " + str(gen_id))
        if len(self._generations) <= 0:
            # Use the first selected generation of players
            return Generation(self._first_strategies, gen_id, self._community_id, 0,
                              self._length_of_generations, self._num_of_onlookers, self._observers)
        else:
            # Use the reproduction mechanism to build a new generation from the last
            return self._reproduce(gen_id)

    def _reproduce(self, gen_id: int) -> Generation:
        """
        Use the last generation of players to build a new generation of players
        Uses the roulette wheel selection via stochastic acceptance outline by Lipowski et al. referenced in my report
        :return: The new generation
        :rtype: Generation
        """
        # Find last generation of players details
        last_gen_players = self._generations[-1].get_players()
        maximal_fitness = 0
        for player in last_gen_players:
            if player.fitness > maximal_fitness:
                maximal_fitness = player.fitness
        # Form new generation
        new_gen_strategies: Dict[Strategy, int] = {}
        new_gen_size = 0
        mutation_strategies = [strategy for strategy in self._first_strategies]
        while new_gen_size < self._generation_size:
            # use stochastic acceptance
            selected_player: Player = random.choice(last_gen_players)
            chance_of_reproduction = 1 if maximal_fitness == 0 else selected_player.fitness/maximal_fitness
            if random.random() <= chance_of_reproduction:
                # Randomly mutate some based on a chosen probability
                if random.random() < self._mutation_chance:
                    selected_strategy = random.choice(mutation_strategies)
                else:
                    selected_strategy = selected_player.strategy
                # Count the strategies as they go in
                if selected_strategy in new_gen_strategies:
                    new_gen_strategies[selected_strategy] += 1
                else:
                    new_gen_strategies[selected_strategy] = 1
                new_gen_size += 1
        return Generation(new_gen_strategies, gen_id, self._community_id, self._current_time,
                          self._current_time+self._length_of_generations, self._num_of_onlookers, self._observers)

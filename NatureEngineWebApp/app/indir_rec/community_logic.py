"""community_logic.py: the module for functionality surrounding communities: reproduction,
simulation of a whole tournament,setup of a tournament etc."""

import requests
from flask import current_app
from typing import List, Dict
from .generation_logic import Generation
import random


class CommunityCreationException(Exception):

    def __init__(self, message):
        super().__init__("Error creating community: " + message)


class Community:

    def __init__(self, strategies: List[Dict], num_of_onlookers: int = 5, num_of_generations: int = 10,
                 length_of_generations: int = 30):
        self._community_id = requests.request("PUT", current_app.config['AGENTS_URL'] + 'community').json()['id']
        if num_of_onlookers <= 0:
            raise CommunityCreationException("number of onlookers <= 0")
        if length_of_generations <= 5:
            raise CommunityCreationException("length of generations <= 5")
        if num_of_generations <= 2:
            raise CommunityCreationException("number of generations <= 2")
        self._num_of_onlookers: int = num_of_onlookers
        self._num_of_generations: int = num_of_generations
        self._length_of_generations: int = length_of_generations
        self._first_strategies = strategies
        self._generations: List[Generation] = []
        self._current_time = 0

    def get_id(self):
        return self._community_id

    def get_num_of_onlookers(self) -> int:
        return self._num_of_onlookers

    def get_length_of_generations(self) -> int:
        return self._num_of_onlookers

    def get_generations(self) -> List[Generation]:
        return self._generations

    def simulate(self):
        for i in range(self._num_of_generations):
            generation = self._build_generation()
            generation.simulate()
            self._generations.append(generation)

    def _build_generation(self) -> Generation:
        if len(self._generations) <= 0:
            return Generation(self._first_strategies, len(self._generations), self._community_id, 0,
                              self._length_of_generations, self._num_of_onlookers)
        else:
            return self._reproduce()

    def _reproduce(self) -> Generation:
        # Gather data on the fitness of strategies
        last_gen_players = self._generations[-1].get_players()
        strategy_fitness: List = []
        overall_fitness = 0
        for player in last_gen_players:
            found_strategy=False
            for strategy in strategy_fitness:
                if strategy['strategy'] == player.get_strategy():
                    found_strategy = True
                    strategy['count'] += player.get_fitness()
            if not found_strategy:
                strategy_fitness.append({'strategy': player.get_strategy(), 'count': player.get_fitness()})
            overall_fitness += player.get_fitness()
        # Build choice intervals to get probability of a new player being a certain strategy
        choice_intervals: List[Dict] = []
        current_interval = 0
        for strategy in strategy_fitness:
            j = current_interval + strategy['count']
            while current_interval <= j:
                choice_intervals.append(strategy['strategy'])
                current_interval += 1
        print(len(choice_intervals))
        # Create strategies for players in next generation
        strategies: List[Dict] = []
        for i in range(len(last_gen_players)):
            # Select a strategy for this player
            selected_index = random.randint(0, overall_fitness)
            print(selected_index)
            player_choice: Dict = choice_intervals[selected_index]
            found_strategy = False
            for strategy in strategies:
                if strategy['strategy'] == player_choice:
                    strategy['count'] += 1
                    found_strategy = True
            if not found_strategy:
                strategies.append({'strategy': player_choice, 'count': 1})
        return Generation(strategies, len(self._generations), self._community_id, self._current_time,
                          self._current_time+self._length_of_generations, self._num_of_onlookers)

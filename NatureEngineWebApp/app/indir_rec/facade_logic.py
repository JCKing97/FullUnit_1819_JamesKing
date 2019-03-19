"""facade_logic.py: An interface for other software the wishes to run games of indirect reciprocity"""

__author__ = "James King"

from .community_logic import Community
from .observation_logic import ActionObserver, PlayerObserver, Observer
from .action_logic import Action, InteractionAction
from typing import List, Dict, Union
from .strategy_logic import Strategy


class Results:
    """The results returned by the running of a game, acts as a facade to the more complicated gathering of the stats"""

    def __init__(self, community: Community):
        """
        Create the observers to observe and gather data on the community as well as creating the
         reference for the community
        :param community: The community to get results and stats about
        :type community: Community
        """
        self._action_observer = ActionObserver(community.get_id())
        self._player_observer = PlayerObserver(community.get_id())
        self._observers: List[Observer] = [self._action_observer, self._player_observer]
        self._community = community
        self._populations: List[Dict[int, List]] = []

    @property
    def generations(self) -> List[int]:
        """
        Get a list of the generation ids for this community
        :return: generation id list
        :rtype: List[int]
        """
        return self._action_observer.generations

    @property
    def players(self) -> Dict[int, List[int]]:
        """
        Get a dictionary where the keys are the ids of each generation that point to a list of id's of
        agents that belong to that generation
        :return: Lists of players for each generation
        :rtype: Dict[int, List[int]]
        """
        return self._action_observer.players

    @property
    def observers(self) -> List[Observer]:
        """
        Get a list of observers that either are attached or need to be attached to the community
        :return: observers list
        :rtype: List[Observer]
        """
        return self._observers

    @property
    def actions(self) -> Dict[int, List[Action]]:
        """
        Get a dictionary where the key is the timepoint which points to a list of actions at that timepoint
        :return: a dictionary of actions
        :rtype: Dict[int, List[Action]]
        """
        return self._action_observer.actions

    @property
    def actions_by_generation(self) -> Dict[int, Dict[int, List[Action]]]:
        """
        Get a dictionary where the keys are generation ids, which point to another dictionary where the keys are
        timepoints that point to a list of actions at that timepoint
        :return: organised dictionary of actions
        :rtype: Dict[int, Dict[int, List[Action]]]
        """
        return self._action_observer.actions_by_generation

    @property
    def actions_by_generation_and_player(self) -> Dict[int, Dict[int, Dict[int, Action]]]:
        """
        Get a dictionary where the keys are generation ids that point to a dictionary where they keys are agent ids
        This points to a dictionary where the keys are timepoints that point to the agents action at that timepoint
        :return: organised disctionary of actions
        :rtype: Dict[int, Dict[int, Dict[int, Action]]]
        """
        return self._action_observer.actions_by_generation_and_player

    @property
    def interactions(self) -> Dict[int, InteractionAction]:
        """
        Get a dictionary of the interactions in the community organised by timepoint (the keys)
        :return: organised dictionary of interaction actions
        :rtype: Dict[int, InteractionAction]
        """
        return self._action_observer.interactions

    @property
    def interactions_by_generation(self) -> Dict[int, Dict[int, InteractionAction]]:
        """
        Get a dictionary of the interactions that occurred where the keys are generation ids that point to a dictionary
        where the keys are timepoints that point to a list of interaction actions
        :return:
        """
        return self._action_observer.interactions_by_generation

    @property
    def interactions_by_generation_and_player(self) -> Dict[int, Dict[int, Dict[int, InteractionAction]]]:
        """
        Get a dictionary of interactions organised with keys that are generations that point to a dictionary where
        the keys are agent ids which point to a dictionary where the keys are timepoints that point to the
        interaction action
        :return: organised dictionary of interaction actions
        :rtype: Dict[int, Dict[int, Dict[int, InteractionAction]]]
        """
        return self._action_observer.interactions_by_generation_and_player

    @property
    def cooperation_rate(self) -> Union[int, None]:
        """
        Get the cooperation rate (percentage of interaction actions that are cooperations) of the community or None
        if there has been no interactions
        :return: community's cooperation rate
        :rtype: Union[int, None]
        """
        return self._action_observer.cooperation_rate

    @property
    def cooperation_rate_by_generation(self) -> Dict[int, Union[int, None]]:
        """
        Get the cooperation rate (percentage of interaction actions that are cooperations) of each generation
        of the community. The keys of the dictionary are generation ids or None
        if there has been no interactions
        :return: each generation's cooperation rate
        :rtype: Dict[int, Union[int, None]]
        """
        return self._action_observer.cooperation_rate_by_generation

    @property
    def cooperation_rate_by_generation_and_player(self) -> Dict[int, Dict[int, Union[int, None]]]:
        """
        Get the cooperation rate of each player in a dictionary with keys that are the generations, that point
        to a dictionary where for each agent in that generation that agent's id is a key or None
        if there has been no interactions
        :return: each player's cooperation rate
        :rtype: Dict[int, Dict[int, Union[int, None]]]
        """
        return self._action_observer.cooperation_rate_by_generation_and_player

    @property
    def social_activeness(self) -> Union[int, None]:
        """
        Get the social activeness (percentage of non-donor actions that are gosisp actions) of the whole community or
        None if there has been no interactions
        :return: social activeness of the community as a whole
        :rtype: Union[int, None]
        """
        return self._action_observer.social_activeness

    @property
    def social_activeness_by_generation(self) -> Dict[int, Union[int, None]]:
        """
       Get the social activeness (percentage of non-donor actions that are gosisp actions) of each generation of the
       community as a dictionary where the keys are the generation ids or None if there has been no interactions
       :return: social activeness of each generation
       :rtype: Dict[int, Union[int, None]]
       """
        return self._action_observer.social_activeness_by_generation

    @property
    def social_activeness_by_generation_and_player(self) -> Dict[int, Dict[int, Union[int, None]]]:
        """
        Get the social activeness of each player, organised with keys that are each generation id pointing to
        a dictionary where for each player of that generation the key is that player's id and the value is the social
        activeness
        :return: the social activeness of each player
        :rtype: Dict[int, Dict[int, Union[int, None]]]
        """
        return self._action_observer.social_activeness_by_generation_and_player

    @property
    def positivity_of_gossip_percentage(self) -> Union[int, None]:
        """
        Get the positivity of the gossip (percentage of gossip actions where the content was positive) for the whole
        community or None if there has been no interactions
        :return: positivity of gossip of the community
        :rtype: Union[int, None]
        """
        return self._action_observer.positivity_of_gossip_percentage

    @property
    def positivity_of_gossip_percentage_by_generation(self) -> Dict[int, Union[int, None]]:
        """
        Get the positivity of the gossip (percentage of gossip actions where the content was positive) for each
        generation of the community or None if there has been no interactions
        :return: positivity of gossip of the each generation
        :rtype: Dict[int, Union[int, None]]
        """
        return self._action_observer.positivity_of_gossip_percentage_by_generation

    @property
    def positivity_of_gossip_percentage_by_generation_and_player(self) -> Dict[int, Dict[int, Union[int, None]]]:
        """
        Get the positivity of gossip of each player in the community as a dictionary organised with the keys as each
        generation's id pointing to a dictionary where the keys are the ids of all the agents that point to the
        positivity of gossip for that agent or None if there has been no interactions
        :return: the positivity of each players gossip
        :rtype: Dict[int, Dict[int, Union[int, None]]]
        """
        return self._action_observer.positivity_of_gossip_percentage_by_generation_and_player

    @property
    def corrupted_observations(self) -> bool:
        """
        Have the results been corrupted in any way when observing the communities simulation
        :return: If the observations have been corrupted
        :rtype: bool
        """
        for observer in self._observers:
            if observer.corrupted_observations:
                return True
        return False

    @property
    def community_fitness(self) -> int:
        """
        Get the summation of the fitness of all the players in the community
        :return: fitness of all the community's players
        :rtype: int
        """
        return self._player_observer.community_fitness

    @property
    def fitness_by_generation(self) -> Dict[int, int]:
        """
        Get the summation of the fitness of all the player of each generation as a dictionary where the keys are
        each generations id
        :return: the summation of each generation's players fitness
        :rtype: Dict[int, int]
        """
        return self._player_observer.fitness_by_generation

    @property
    def fitness_by_generation_and_player(self) -> Dict[int, Dict[int, int]]:
        """
        Get the fitness of each player organised in a dictionary where the keys are generation ids which point to
        another dictionary in which the keys are player ids that point to the fitness of that player
        :return: the fitness of each player
        :rtype: Dict[int, Dict[int, int]]
        """
        return self._player_observer.fitness_by_generation_and_player

    @property
    def populations(self) -> List[Dict[Strategy, int]]:
        """
        Get the count of each strategy for each generation to see how the population of strategies fluctuates
        :return: The population in terms of strategies of each generation
        :rtype: List[Dict[Strategy, int]]
        """
        return self._community.get_strategy_count_by_generation()

    @property
    def id_to_strategy_map(self) -> Dict[int, Dict[int, Strategy]]:
        """
        Get a map which maps the id of each player to it's strategy
        :return: dictionary which maps players to strategies
        :rtype: Dict[int, Dict[int, Strategy]]
        """
        map: Dict = {}
        for gen in self._community.get_generations():
            map[gen.id] = {}
            for player in gen.get_players():
                map[gen.id][player.id] = player.strategy
        return map


class ReputationGame:
    """The facade for a game of the theoretical framework I have laid out in my report"""

    def __init__(self, initial_strategies: List[Dict], num_of_onlookers: int = 5, num_of_generations: int = 10,
                 length_of_generations: int = 30, mutation_chance: float = 0):
        """
        Create a new reputation game with the parameters passed
        :param initial_strategies: A list of the strategies to use in the first generation of the community
        :type initial_strategies: List[Dict]
        :param num_of_onlookers: the number of onlookers for each interaction (defaults to 5)
        :type num_of_onlookers: int
        :param num_of_generations: The number of generations that will be part of this community when run (defaults to
        10)
        :type num_of_generations: int
        :param length_of_generations: The number of timepoints in each generation (defaults to 30)
        :type length_of_generations: int
        :param mutation_chance: The chance for mutation to occur in the reproduction of any one player
        :type mutation_chance: float
        """
        self._initial_strategies = initial_strategies
        self._num_of_onlookers = num_of_onlookers
        self._num_of_generations = num_of_generations
        self._length_of_generations = length_of_generations
        self._mutation_chance = mutation_chance

    @property
    def initial_strategies(self) -> List[Dict]:
        """
        Get the set of strategies set for the first generation of this community
        :return: The strategies of the first gen
        :rtype: List[Dict]
        """
        return self._initial_strategies

    @property
    def num_of_onlookers(self) -> int:
        """
        Get the number of onlookers for each interaction in this community
        :return: number of onlookers per interaction
        :rtype: int
        """
        return self._num_of_onlookers

    @property
    def num_of_generations(self) -> int:
        """
        Get the number of generations that has been played or is to be played in this game
        :return: number of generations of the game
        :rtype: int
        """
        return self._num_of_generations

    @property
    def length_of_generations(self) -> int:
        """
        Get the number of timepoints in each generation
        :return: length of each generation
        :rtype: int
        """
        return self._length_of_generations

    @property
    def mutation_chance(self) -> float:
        """
        Get the chance of mutation for each players strategy in the system
        :return: the chance of mutation in each reproduction
        :rtype: float
        """
        return self._mutation_chance

    def run(self) -> Results:
        """
        Run the game and observe it, returning the observations and results (the result won't be the same each time)
        :return: the statistics and results of each game
        :rtype: Results
        """
        community_strategies: Dict[Strategy, int] = {}
        # Create the first generations strategies from the initial_strategies dictionary
        for strategy in self._initial_strategies:
            generated_strategy: Strategy = Strategy(strategy['donor_strategy'], strategy['non_donor_strategy'],
                                                    strategy['trust_model'], strategy['options'])
            if generated_strategy in community_strategies:
                community_strategies[generated_strategy] += strategy['count']
            else:
                community_strategies[generated_strategy] = strategy['count']
        # Create the community to simulate
        community = Community(community_strategies, num_of_onlookers=self._num_of_onlookers,
                              num_of_generations=self._num_of_generations,
                              length_of_generations=self._length_of_generations,
                              mutation_chance=self._mutation_chance)
        # Create the results object and add the observers to the community then simulate
        results = Results(community)
        community.extend_observers(results.observers)
        community.simulate()
        return results


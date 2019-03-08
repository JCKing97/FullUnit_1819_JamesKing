"""facade_tests.py: Testing for the facades placed on top of the whole system of running and getting the results of
a reputation game"""

import unittest
from app import create_app
from tests.test_config import TestConfig
import requests
import random
import pprint
from .facade_logic import Results, ReputationGame
from .observation_logic import ActionObserver, PlayerObserver
from .action_logic import Action
from .strategy_logic import Strategy


class FacadeTests(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        response = requests.get('http://127.0.0.1:8080/strategy')
        cls.received_strategies = response.json()['strategies']
        cls.strat_count = 0
        cls.strategies = []
        cls.type_strategies = []
        for strategy in cls.received_strategies:
            count = random.randint(0, 1)
            cls.strategies.append({'donor_strategy': strategy['donor_strategy'], 'non_donor_strategy':
                                    strategy['non_donor_strategy'], 'trust_model': strategy['trust_model'],
                                   'options': strategy['options'], 'count': count})
            cls.type_strategies.append(Strategy(strategy['donor_strategy'], strategy['non_donor_strategy'],
                                                    strategy['trust_model'], strategy['options']))
            cls.strat_count += count
        cls.pp = pprint.PrettyPrinter()
        cls.num_of_onlookers = random.randint(1, 10)
        cls.num_of_generations = random.randint(3, 5)
        cls.length_of_generations = random.randint(6, 10)
        cls.mutation_chance = random.random()
        print(cls.strategies)
        cls.reputation_game: ReputationGame = ReputationGame(cls.strategies, cls.num_of_onlookers,
                                                             cls.num_of_generations, cls.length_of_generations,
                                                             cls.mutation_chance)
        cls.app = create_app(TestConfig)
        cls.app_context = cls.app.app_context()
        cls.app_context.push()
        cls.results: Results = cls.reputation_game.run()

    def setUp(self):
        self.app = create_app(TestConfig)
        self.app_context = self.app.app_context()
        self.app_context.push()

    def test_corruption(self):
        self.assertEqual(False, self.results.corrupted_observations, "Observations should not have corrupted "
                                                                     "when running")

    def test_generations(self):
        # Check the amount of generations
        generations = self.results.generations
        self.assertEqual(self.num_of_generations, len(generations), "There should be the number of generations"
                                                                    " originally specified")

    def test_players(self):
        generations = self.results.generations
        players = self.results.players
        for generation in generations:
            # Check the amount of players per generation
            self.assertEqual(self.strat_count, len(players[generation]), "Each generation should have the same amount "
                                                                         "of players as first set out")

    def test_observer_presence(self):
        # Check the presence of action and player observers
        observers = self.results.observers
        observer_type = [type(observer) for observer in observers]
        if [ActionObserver, PlayerObserver] != observer_type and [PlayerObserver, ActionObserver] != observer_type:
            self.fail("Should have one action observer and one player observer")

    def test_community_actions(self):
        # Check the timepoints in the actions dict and also the amount of actions per timepoint
        actions = self.results.actions
        self.pp.pprint(actions)
        print(self.num_of_generations)
        print(self.length_of_generations)
        action_count = 0
        for timepoint in actions:
            action_count += len(actions[timepoint])
        self.assertEqual(self.num_of_generations*self.length_of_generations*self.strat_count, action_count,
                         "There should be as many actions as number of generations*length of generations* amount of "
                         "players in each generation")
        self.assertEqual(self.num_of_generations*self.length_of_generations, len(actions),
                         "There should be as many timepoints as the length of a generation multiplied "
                         "by the number of generations")
        for timepoint in actions:
            self.assertEqual(self.strat_count, len(actions[timepoint]), "There should be as many actions as timepoints "
                                                                        "multiple by the number of players")

    def test_community_interactions(self):
        # Check that there is an interaction for each timepoint
        interactions = self.results.interactions
        self.pp.pprint(interactions)
        self.assertEqual(self.num_of_generations*self.length_of_generations, len(interactions),
                         "There should be as many interactions as there are timepoints")
        for timepoint in interactions:
            self.assertTrue(Action in interactions[timepoint].__class__.__bases__,
                            "There should be one interaction for each timepoint")

    def test_actions_by_generation(self):
        actions_by_generation = self.results.actions_by_generation
        players = self.results.players
        generations = self.results.generations
        self.pp.pprint(actions_by_generation)
        for generation in generations:
            # Test the actions and interactions by generation dict size
            # and shape for timepoints and actions per timepoint
            self.assertEqual(self.length_of_generations, len(actions_by_generation[generation]),
                             "There should be as many timepoints in a generation as the length of the generation")
            for timepoint in actions_by_generation[generation]:
                self.assertEqual(self.strat_count, len(actions_by_generation[generation][timepoint]),
                                 "There should be as many actions per timepoint as there are agents")

    def test_actions_by_generation_and_player(self):
        actions_by_generation_and_player = self.results.actions_by_generation_and_player
        players = self.results.players
        generations = self.results.generations
        for generation in generations:
            for player in players[generation]:
                # Check the actions carried out by players
                self.assertEqual(self.length_of_generations, len(actions_by_generation_and_player[generation][player]),
                                 "An agent should carry out an action for each timepoint in their generation")
                for timepoint in actions_by_generation_and_player[generation][player]:
                    self.assertTrue(Action in actions_by_generation_and_player[generation][player][timepoint].__class__.__bases__,
                                    "An agent should carry out one action for each timepoint in their generation")

    def test_interactions_by_generation(self):
        interactions_by_generation = self.results.interactions_by_generation
        generations = self.results.generations
        for generation in generations:
            self.assertEqual(self.length_of_generations, len(interactions_by_generation[generation]),
                             "There should be as many timepoints for interactions in a generations as the length "
                             "of the generation")
            for timepoint in interactions_by_generation[generation]:
                self.assertTrue(Action in interactions_by_generation[generation][timepoint].__class__.__bases__,
                                "There should be 1 interaction per timepoint")

    def test_cooperation_rate(self):
        self.assertLessEqual(self.results.cooperation_rate, 100, "Should be a percentage therefore le 100")
        self.assertGreaterEqual(self.results.cooperation_rate, 0, "Should be a percentage therefore ge 0")

    def test_cooperation_rate_by_generation(self):
        generations = self.results.generations
        cooperation_rate_by_gen = self.results.cooperation_rate_by_generation
        self.assertEqual(self.num_of_generations, len(cooperation_rate_by_gen))
        for generation in generations:
            self.assertLessEqual(cooperation_rate_by_gen[generation], 100, "Should be a percentage therefore le 100")
            self.assertGreaterEqual(cooperation_rate_by_gen[generation], 0, "Should be a percentage therefore ge 0")

    def test_cooperation_rate_by_gen_and_player(self):
        generations = self.results.generations
        players = self.results.players
        cooperation_rate_by_gen_and_player = self.results.cooperation_rate_by_generation_and_player
        for generation in generations:
            self.assertEqual(self.strat_count, len(cooperation_rate_by_gen_and_player[generation]))
            for player in players[generation]:
                if cooperation_rate_by_gen_and_player[generation][player] is not None:
                    self.assertLessEqual(cooperation_rate_by_gen_and_player[generation][player], 100,
                                         "Should be a percentage therefore le 100")
                    self.assertGreaterEqual(cooperation_rate_by_gen_and_player[generation][player], 0,
                                            "Should be a percentage therefore ge 0")

    def test_social_activeness(self):
        self.assertLessEqual(self.results.social_activeness, 100, "Should be a percentage therefore le 100")
        self.assertGreaterEqual(self.results.social_activeness, 0, "Should be a percentage therefore ge 0")

    def test_social_activeness_by_gen(self):
        generations = self.results.generations
        social_activeness_by_gen = self.results.social_activeness_by_generation
        self.assertEqual(self.num_of_generations, len(social_activeness_by_gen))
        for generation in generations:
            self.assertLessEqual(social_activeness_by_gen[generation], 100, "Should be a percentage therefore le 100")
            self.assertGreaterEqual(social_activeness_by_gen[generation], 0, "Should be a percentage therefore ge 0")

    def test_social_activeness_by_gen_and_player(self):
        generations = self.results.generations
        players = self.results.players
        social_activeness_by_gen_and_player = self.results.social_activeness_by_generation_and_player
        for generation in generations:
            self.assertEqual(self.strat_count, len(social_activeness_by_gen_and_player[generation]))
            for player in players[generation]:
                self.assertLessEqual(social_activeness_by_gen_and_player[generation][player], 100,
                                     "Should be a percentage therefore le 100")
                self.assertGreaterEqual(social_activeness_by_gen_and_player[generation][player], 0,
                                        "Should be a percentage therefore ge 0")

    def test_positivity_of_gossip(self):
        self.assertLessEqual(self.results.positivity_of_gossip_percentage, 100,
                             "Should be a percentage therefore le 100")
        self.assertGreaterEqual(self.results.positivity_of_gossip_percentage, 0,
                                "Should be a percentage therefore ge 0")

    def test_positivity_of_gossip_percentage_by_gen(self):
        generations = self.results.generations
        positivity_of_gossip_percentage_by_gen = self.results.positivity_of_gossip_percentage_by_generation
        self.assertEqual(self.num_of_generations, len(positivity_of_gossip_percentage_by_gen))
        for generation in generations:
            self.assertLessEqual(positivity_of_gossip_percentage_by_gen[generation], 100,
                                 "Should be a percentage therefore le 100")
            self.assertGreaterEqual(positivity_of_gossip_percentage_by_gen[generation], 0,
                                    "Should be a percentage therefore ge 0")

    def test_positivity_of_gossip_percentage_by_gen_and_player(self):
        generations = self.results.generations
        players = self.results.players
        positivity_of_gossip_percentage_by_gen_and_player = \
            self.results.positivity_of_gossip_percentage_by_generation_and_player
        for generation in generations:
            self.assertEqual(self.strat_count, len(positivity_of_gossip_percentage_by_gen_and_player[generation]))
            for player in players[generation]:
                if positivity_of_gossip_percentage_by_gen_and_player[generation][player] is not None:
                    self.assertLessEqual(positivity_of_gossip_percentage_by_gen_and_player[generation][player], 100,
                                         "Should be a percentage therefore le 100")
                    self.assertGreaterEqual(positivity_of_gossip_percentage_by_gen_and_player[generation][player], 0,
                                            "Should be a percentage therefore ge 0")

    def test_community_fitness(self):
        self.assertGreaterEqual(self.results.community_fitness, 0, "Community fitness should be positive")

    def test_fitness_by_gen(self):
        fitness_total = 0
        generations = self.results.generations
        fitness_by_gen = self.results.fitness_by_generation
        for generation in generations:
            self.assertGreaterEqual(fitness_by_gen[generation], 0, "Should be positive")
            fitness_total += fitness_by_gen[generation]
        self.assertEqual(self.results.community_fitness, fitness_total,
                         "The total of the fitness of each generation should add up to the overall community fitness")

    def test_fitness_by_gen_and_player(self):
        fitness_total = 0
        fitness_by_gen = self.results.fitness_by_generation
        fitness_by_gen_and_players = self.results.fitness_by_generation_and_player
        generations = self.results.generations
        players = self.results.players
        for generation in generations:
            generation_fitness = 0
            for player in players[generation]:
                self.assertGreaterEqual(fitness_by_gen_and_players[generation][player], 0,
                                        "Should have a positive fitness")
                fitness_total += fitness_by_gen_and_players[generation][player]
                generation_fitness += fitness_by_gen_and_players[generation][player]
            self.assertEqual(fitness_by_gen[generation], generation_fitness,
                             "The total fitness of all the players of a generation should add up to the generation "
                             "fitness")

        self.assertEqual(self.results.community_fitness, fitness_total,
                         "The total of the fitness of each generation should add up to the overall community fitness")

    def test_populations(self):
        generations = self.results.generations
        populations = self.results.populations
        print(populations)
        self.assertEqual(self.num_of_generations, len(populations))
        self.pp.pprint(self.strategies)
        strategies = [strategy for strategy in self.strategies]
        for generation in generations:
            strat_count = 0
            for strategy in populations[generation]:
                print(strategy.__str__())
                this_strat_count = populations[generation][strategy]
                print(this_strat_count)
                self.assertGreaterEqual(this_strat_count, 0, "Should be greater than 1 of each strategy")
                strat_count += this_strat_count
                self.assertTrue(strategy in self.type_strategies, "Should correspond to a strategy in the agents service")
            self.assertEqual(self.strat_count, strat_count, "Should be the same amount of players for each generation")

    def test_id_to_strat_map(self):
        generations = self.results.generations
        players = self.results.players
        populations = self.results.populations
        id_to_strat_map = self.results.id_to_strategy_map
        self.pp.pprint(id_to_strat_map)
        # id_to_populations_community = {}
        # for generation in generations:
        #     ids_to_population_generation = []
        #     for player in players[generation]:
        #         found_strategy = False
        #         for strategy in ids_to_population_generation:
        #             if strategy['strategy'] == id_to_strat_map[player]:
        #                 found_strategy = True
        #                 strategy['count'] += 1
        #         if not found_strategy:
        #         ids_to_population_generation = {'strategy': }
        #     id_to_populations_community[generation] = ids_to_population_generation









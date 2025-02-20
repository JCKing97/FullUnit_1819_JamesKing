"""run_experiments.py: A script to run experiments with the game"""

__author__ = "James King"

import json
import time
from app.indir_rec.facade_logic import *


def run_stability_experiments(experiments_json_filename, strategies_json_filename, output_file_path):
    experiments = json.loads(open(experiments_json_filename).read())
    strategies = json.loads(open(strategies_json_filename).read())
    strategy_num = 0
    for strategy in strategies:
        print("New strategy: " + str(strategy_num))
        experiment_num = 0
        for experiment in experiments:
            print("Experiment: " + str(experiment_num))
            output_filename = "strat_num=" + str(strategy_num) + ",experiment_num=" + str(experiment_num) + ":" \
                              + str(time.time()) + ".json"
            strategy['count'] = 25
            population = [strategy,
                          {'donor_strategy': 'Defector', 'non_donor_strategy': 'Lazy', 'trust_model': 'Void',
                           'options': [], 'count': 2},
                          {'donor_strategy': 'Defector', 'non_donor_strategy': 'Promote Self', 'trust_model': 'Void',
                           'options': [], 'count': 2},
                          {'donor_strategy': 'Defector', 'non_donor_strategy': 'Spread Negative', 'trust_model': 'Void',
                           'options': [], 'count': 2}]
            results: Results = ReputationGame(population, experiment['num_of_onlookers'], experiment['gen_num'],
                                              experiment['gen_length'], 0).run()
            json_write_data = {'experiment': experiment, 'coop_rate': results.cooperation_rate,
                               'coop_rate_by_gen': results.cooperation_rate_by_generation,
                               'social_activeness': results.social_activeness,
                               'social_activeness_by_gen': results.social_activeness_by_generation,
                               'positivity_of_gossip_percentage': results.positivity_of_gossip_percentage,
                               'positivity_of_gossip_percentage_by_gen':
                                   results.positivity_of_gossip_percentage_by_generation,
                               'corrupted_observations': results.corrupted_observations,
                               'community_fitness': results.community_fitness,
                               'fitness_by_generation': results.fitness_by_generation}
            populations_data = []
            for generation_population in results.populations:
                populations_data.append([{'strategy': strategy.to_dict(), 'count': count} for strategy, count
                                         in generation_population.items()])
            json_write_data['populations'] = populations_data
            with open(output_file_path + output_filename, "w+") as output_file:
                json.dump(json_write_data, output_file, indent=4)
            experiment_num += 1
        strategy_num += 1


def run_robustness_experiments(experiments_json_filename, population_json_filename, output_file_path):
    experiments = json.loads(open(experiments_json_filename).read())
    populations = json.loads(open(population_json_filename).read())
    experiment_num = 1
    for experiment in experiments:
        print("Experiment: "+str(experiment_num))
        output_filename = "gen_length="+str(experiment['gen_length'])+",gen_num="+str(experiment['gen_num'])+\
                          ",num_of_onlookers="+str(experiment['num_of_onlookers'])+":"+str(time.time())+".json"
        results: Results = ReputationGame(populations, experiment['num_of_onlookers'], experiment['gen_num'],
                                          experiment['gen_length'], 0).run()
        json_write_data = {'experiment': experiment, 'coop_rate': results.cooperation_rate,
                           'coop_rate_by_gen': results.cooperation_rate_by_generation,
                           'social_activeness': results.social_activeness,
                           'social_activeness_by_gen': results.social_activeness_by_generation,
                           'positivity_of_gossip_percentage': results.positivity_of_gossip_percentage,
                           'positivity_of_gossip_percentage_by_gen':
                               results.positivity_of_gossip_percentage_by_generation,
                           'corrupted_observations': results.corrupted_observations,
                           'community_fitness': results.community_fitness,
                           'fitness_by_generation': results.fitness_by_generation}
        populations_data = []
        for generation_population in results.populations:
            populations_data.append([{'strategy': strategy.to_dict(), 'count': count} for strategy, count
                                     in generation_population.items()])
        json_write_data['populations'] = populations_data
        with open(output_file_path+output_filename, "w+") as output_file:
            json.dump(json_write_data, output_file, indent=4)
        experiment_num += 1



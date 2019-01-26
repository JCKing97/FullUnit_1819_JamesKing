"""run_game.py: contains the logic to throw a reputation game into a redis queue"""

from .facade_logic import ReputationGame, Results
from ..models import ReputationAction, ReputationCommunity, ReputationGeneration, ReputationPlayer, ReputationStrategy
from app import db
from .action_logic import ActionType, InteractionAction, GossipAction


def reputation_run(strategies, num_of_onlookers, num_of_generations, length_of_generations, mutation_chance):
    """Run a reputation game and store the results in a database"""
    game: ReputationGame = ReputationGame(strategies, num_of_onlookers, num_of_generations,
                                          length_of_generations, mutation_chance)
    game_results: Results = game.run()
    commit_results_game_to_database(game, game_results)


def commit_results_game_to_database(game: ReputationGame, game_results: Results):
    """Store the results of a reputation game in the database"""
    if game_results.corrupted_observations:
        # If the results are corrupted don't add them to the database, just note that the observations were corrupted
        community = ReputationCommunity(corrupted_observations=True)
        db.session.add(community)
        db.session.flush()
    else:
        # Add the community to the database and its stats
        community = ReputationCommunity(corrupted_observations=False, number_of_onlookers=game.num_of_onlookers,
                                        length_of_generations=game.length_of_generations,
                                        mutation_chance=game.mutation_chance,
                                        cooperation_rate=game_results.cooperation_rate,
                                        social_activeness=game_results.social_activeness,
                                        positivity_of_gossip=game_results.positivity_of_gossip_percentage,
                                        fitness=game_results.community_fitness)
        db.session.add(community)
        db.session.flush()
        # Get all the results and statistics in one go as it is more efficient
        cooperation_by_gen = game_results.cooperation_rate_by_generation
        cooperation_by_gen_and_player = game_results.cooperation_rate_by_generation_and_player
        fitness_by_gen = game_results.fitness_by_generation
        fitness_by_gen_and_player = game_results.fitness_by_generation_and_player
        social_activeness_by_gen = game_results.social_activeness_by_generation
        social_activeness_by_gen_and_player = game_results.social_activeness_by_generation_and_player
        positivity_of_gossip_by_gen = game_results.positivity_of_gossip_percentage_by_generation
        positivity_of_gossip_by_gen_and_player = game_results.positivity_of_gossip_percentage_by_generation_and_player
        actions_by_generation_and_player = game_results.actions_by_generation_and_player
        actions_by_generation = game_results.actions_by_generation
        id_to_strat_map = game_results.id_to_strategy_map
        generations = game_results.generations
        players = game_results.players
        for generation in generations:
            # Add each generation from the community to the database with all the gens stats
            new_gen = ReputationGeneration(community.id, id=generation,
                                           start_point=min(actions_by_generation[generation]),
                                           end_point=max(actions_by_generation[generation]),
                                           cooperation_rate=cooperation_by_gen[generation],
                                           social_activeness=social_activeness_by_gen[generation],
                                           positivity_of_gossip=positivity_of_gossip_by_gen[generation],
                                           fitness=fitness_by_gen[generation])
            db.session.add(new_gen)
            db.session.flush()
            for player in players[generation]:
                # Add each player from the generation to database with all their stats
                player_strat = id_to_strat_map[generation][player]
                # If the strategy for the player doesn't already exist in the database add it
                if ReputationStrategy.query.filter(strategy_name=player_strat['name'],
                                                   strategy_options=player_strat['options']).count() <= 0:
                    new_strat = ReputationStrategy(strategy_name=player_strat['name'],
                                                   strategy_options=player_strat['options'])
                    db.session.add(new_strat)
                    db.session.flush()
                new_player = ReputationPlayer(generation_id=new_gen.id, community_id=community.id,
                                              cooperation_rate=cooperation_by_gen_and_player[generation][player],
                                              social_activeness=social_activeness_by_gen_and_player[generation][player],
                                              positivity_of_gossip=
                                              positivity_of_gossip_by_gen_and_player[generation][player],
                                              fitness=fitness_by_gen_and_player[generation][player],
                                              strategy_name=player_strat['name'],
                                              strategy_options=player_strat['options'])
                db.session.add(new_player)
                db.session.flush()
                # Add all the actions the player committed to and their details to the database
                for timepoint in actions_by_generation_and_player[generation][player]:
                    if actions_by_generation_and_player[generation][player][timepoint].type is ActionType.INTERACTION:
                        interaction: InteractionAction = actions_by_generation_and_player[generation][player][timepoint]
                        new_action = ReputationAction(generation_id=new_gen.id, community_id=community.id,
                                                      player_id=new_player.id, id=timepoint, timepoint=timepoint,
                                                      type=ActionType.INTERACTION, donor=interaction.donor,
                                                      recipient=interaction.recipient, onlookers=interaction.onlookers,
                                                      action=interaction.action)
                    elif actions_by_generation_and_player[generation][player][timepoint].type is ActionType.GOSSIP:
                        gossip: GossipAction = actions_by_generation_and_player[generation][player][timepoint]
                        new_action = ReputationAction(generation_id=new_gen.id, community_id=community.id,
                                                      player_id=new_player.id, id=timepoint, timepoint=timepoint,
                                                      type=ActionType.GOSSIP, gossiper=gossip.gossiper,
                                                      about=gossip.about, recipient=gossip.recipient,
                                                      gossip=gossip.gossip)
                    else:
                        new_action = ReputationAction(generation_id=new_gen.id, community_id=community.id,
                                                      player_id=new_player.id, id=timepoint, timepoint=timepoint,
                                                      type=ActionType.IDLE)
                    db.session.add(new_action)
                    db.session.flush()

"""run_game.py: contains the logic to throw a reputation game into a redis queue"""

from .facade_logic import ReputationGame, Results
from ..models import ReputationAction, ReputationCommunity, ReputationGeneration, ReputationPlayer, ReputationStrategy,\
    ReputationActionOnlookers, Experiment
from app import db, create_app
from .action_logic import ActionType, InteractionAction, GossipAction
import json

app = create_app()
app.app_context().push()


def reputation_run(strategies, num_of_onlookers, num_of_generations, length_of_generations, mutation_chance,
                   database_community_id, user_id=None, label=None):
    """Run a reputation game and store the results in a database"""
    game: ReputationGame = ReputationGame(strategies, num_of_onlookers, num_of_generations,
                                          length_of_generations, mutation_chance)
    game_results: Results = game.run()
    commit_results_game_to_database(game, game_results, database_community_id, user_id, label)


def commit_results_game_to_database(game: ReputationGame, game_results: Results, database_community_id, user_id, label):
    """Store the results of a reputation game in the database"""
    community: ReputationCommunity = ReputationCommunity.query.filter_by(id=database_community_id).first()
    if user_id is not None and label is not None:
        experiment: Experiment = Experiment(community_id=database_community_id, user_id=user_id, label=label)
        db.session.add(experiment)
        db.session.commit()
    if game_results.corrupted_observations:
        # If the results are corrupted don't add them to the database, just note that the observations were corrupted
        community.set_corrupted()
    else:
        # Update the community in the database
        community.set_not_corrupted(number_of_onlookers=game.num_of_onlookers,
                                    length_of_generations=game.length_of_generations,
                                    mutation_chance=game.mutation_chance,
                                    cooperation_rate=game_results.cooperation_rate,
                                    social_activeness=game_results.social_activeness,
                                    positivity_of_gossip=game_results.positivity_of_gossip_percentage,
                                    fitness=game_results.community_fitness)
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
            new_gen = ReputationGeneration(community_id=community.id, generation_id=generation,
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
                # Having to use a string representation as sqlite doesn't support arrays
                strategy_options_string = json.dumps(player_strat.options)
                # If the strategy for the player doesn't already exist in the database add it
                if ReputationStrategy.query.filter_by(strategy_name=player_strat.name,
                                                      strategy_options=strategy_options_string).count() <= 0:
                    new_strat = ReputationStrategy(strategy_name=player_strat.name,
                                                   strategy_options=strategy_options_string)
                    db.session.add(new_strat)
                    db.session.flush()
                player_strategy = ReputationStrategy.query.filter_by(strategy_name=player_strat.name,
                                                                     strategy_options=strategy_options_string).first()
                new_player = ReputationPlayer(generation_id=new_gen.id, community_id=community.id, player_id=player,
                                              cooperation_rate=cooperation_by_gen_and_player[generation][player],
                                              social_activeness=social_activeness_by_gen_and_player[generation][player],
                                              positivity_of_gossip=
                                              positivity_of_gossip_by_gen_and_player[generation][player],
                                              fitness=fitness_by_gen_and_player[generation][player],
                                              strategy=player_strategy.id)
                db.session.add(new_player)
                db.session.flush()
                # Add all the actions the player committed to and their details to the database
                for timepoint in actions_by_generation_and_player[generation][player]:
                    if actions_by_generation_and_player[generation][player][timepoint].type is ActionType.INTERACTION:
                        interaction: InteractionAction = actions_by_generation_and_player[generation][player][timepoint]
                        new_action = ReputationAction(generation_id=new_gen.id, community_id=community.id,
                                                      player_id=new_player.id, timepoint=timepoint,
                                                      type=ActionType.INTERACTION, donor=interaction.donor,
                                                      recipient=interaction.recipient, action=interaction.action)
                        db.session.add(new_action)
                        db.session.flush()
                        for onlooker in interaction.onlookers:
                            reputation_action_onlooker = ReputationActionOnlookers(community_id=community.id,
                                                                                   generation_id=new_gen.id,
                                                                                   actor_id=new_player.id,
                                                                                   onlooker_id=onlooker,
                                                                                   action_id=timepoint)
                            db.session.add(reputation_action_onlooker)
                            db.session.flush()
                    elif actions_by_generation_and_player[generation][player][timepoint].type is ActionType.GOSSIP:
                        gossip: GossipAction = actions_by_generation_and_player[generation][player][timepoint]
                        new_action = ReputationAction(generation_id=new_gen.id, community_id=community.id,
                                                      player_id=new_player.id, timepoint=timepoint,
                                                      type=ActionType.GOSSIP, gossiper=gossip.gossiper,
                                                      about=gossip.about, recipient=gossip.recipient,
                                                      gossip=gossip.gossip)
                        db.session.add(new_action)
                        db.session.flush()
                    else:
                        new_action = ReputationAction(generation_id=new_gen.id, community_id=community.id,
                                                      player_id=new_player.id, timepoint=timepoint,
                                                      type=ActionType.IDLE)
                        db.session.add(new_action)
                        db.session.flush()
    community.simulated = True
    db.session.commit()

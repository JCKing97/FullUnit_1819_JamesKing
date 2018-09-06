def strat_2(db, Strategy):
    db.session.add_all([Strategy(name="Tit-For-Tat", description='Start by cooperating, then copy the last turn from the other agent'),
                        Strategy(name="Grudger", description='Start by cooperating, if the other agent defects switch and defect from then on'),
                        Strategy(name="Always Cooperate", description='Cooperate no matter what'),
                        Strategy(name="Always Defect", description='Defect no matter what')])
    db.session.commit()


def games_2(db, Game, Agent, Action, GameAgentAssociation):
    game1 = Game()
    game2 = Game()
    game3 = Game()
    agent1 = Agent(strategy_name='Tit-For-Tat')
    agent2 = Agent(strategy_name='Grudger')
    agent3 = Agent(strategy_name='Always Cooperate')
    agent4 = Agent(strategy_name='Always Defect')
    db.session.add_all([game1, game2, game3, agent1, agent2, agent3, agent4])
    db.session.commit()
    agent_game_associations = [GameAgentAssociation(game_id=game1.id, agent_id=agent1.id),
                               GameAgentAssociation(game_id=game1.id, agent_id=agent2.id),
                               GameAgentAssociation(game_id=game2.id, agent_id=agent3.id),
                               GameAgentAssociation(game_id=game2.id, agent_id=agent4.id),
                               GameAgentAssociation(game_id=game3.id, agent_id=agent2.id),
                               GameAgentAssociation(game_id=game3.id, agent_id=agent4.id)]
    actions = [Action(game_id=game1.id, round_num=1, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=1, agent_id=agent2.id, cooperates=True),
               Action(game_id=game1.id, round_num=2, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=2, agent_id=agent2.id, cooperates=True),
               Action(game_id=game1.id, round_num=3, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=3, agent_id=agent2.id, cooperates=True),
               Action(game_id=game1.id, round_num=4, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=4, agent_id=agent2.id, cooperates=True),
               Action(game_id=game2.id, round_num=1, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=1, agent_id=agent4.id, cooperates=False),
               Action(game_id=game2.id, round_num=2, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=2, agent_id=agent4.id, cooperates=False),
               Action(game_id=game2.id, round_num=3, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=3, agent_id=agent4.id, cooperates=False),
               Action(game_id=game2.id, round_num=4, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=4, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=1, agent_id=agent2.id, cooperates=True),
               Action(game_id=game3.id, round_num=1, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=2, agent_id=agent2.id, cooperates=False),
               Action(game_id=game3.id, round_num=2, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=3, agent_id=agent2.id, cooperates=False),
               Action(game_id=game3.id, round_num=3, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=4, agent_id=agent2.id, cooperates=False),
               Action(game_id=game3.id, round_num=4, agent_id=agent4.id, cooperates=False)]
    db.session.add_all(agent_game_associations)
    db.session.add_all(actions)
    db.session.commit()

def tournament(db, Game, Agent, Action, GameAgentAssociation, Tournament):
    tournament = Tournament()
    db.session.add(tournament)
    db.session.commit()
    game1 = Game(tournament=tournament.id)
    game2 = Game(tournament=tournament.id)
    game3 = Game(tournament=tournament.id)
    game4 = Game(tournament=tournament.id)
    game5 = Game(tournament=tournament.id)
    game6 = Game(tournament=tournament.id)
    agent1 = Agent(strategy_name='Tit-For-Tat')
    agent2 = Agent(strategy_name='Grudger')
    agent3 = Agent(strategy_name='Always Cooperate')
    agent4 = Agent(strategy_name='Always Defect')
    db.session.add_all([game1, game2, game3, game4, game5, game6, agent1, agent2, agent3, agent4])
    db.session.commit()
    agent_game_associations = [GameAgentAssociation(game_id=game1.id, agent_id=agent1.id),
                               GameAgentAssociation(game_id=game1.id, agent_id=agent2.id),
                               GameAgentAssociation(game_id=game2.id, agent_id=agent1.id),
                               GameAgentAssociation(game_id=game2.id, agent_id=agent3.id),
                               GameAgentAssociation(game_id=game3.id, agent_id=agent1.id),
                               GameAgentAssociation(game_id=game3.id, agent_id=agent4.id),
                               GameAgentAssociation(game_id=game4.id, agent_id=agent2.id),
                               GameAgentAssociation(game_id=game4.id, agent_id=agent3.id),
                               GameAgentAssociation(game_id=game5.id, agent_id=agent2.id),
                               GameAgentAssociation(game_id=game5.id, agent_id=agent4.id),
                               GameAgentAssociation(game_id=game6.id, agent_id=agent3.id),
                               GameAgentAssociation(game_id=game6.id, agent_id=agent4.id)]
    actions = [Action(game_id=game1.id, round_num=1, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=1, agent_id=agent2.id, cooperates=True),
               Action(game_id=game1.id, round_num=2, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=2, agent_id=agent2.id, cooperates=True),
               Action(game_id=game1.id, round_num=3, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=3, agent_id=agent2.id, cooperates=True),
               Action(game_id=game1.id, round_num=4, agent_id=agent1.id, cooperates=True),
               Action(game_id=game1.id, round_num=4, agent_id=agent2.id, cooperates=True),
               Action(game_id=game2.id, round_num=1, agent_id=agent1.id, cooperates=True),
               Action(game_id=game2.id, round_num=1, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=2, agent_id=agent1.id, cooperates=True),
               Action(game_id=game2.id, round_num=2, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=3, agent_id=agent1.id, cooperates=True),
               Action(game_id=game2.id, round_num=3, agent_id=agent3.id, cooperates=True),
               Action(game_id=game2.id, round_num=4, agent_id=agent1.id, cooperates=True),
               Action(game_id=game2.id, round_num=4, agent_id=agent3.id, cooperates=True),
               Action(game_id=game3.id, round_num=1, agent_id=agent1.id, cooperates=True),
               Action(game_id=game3.id, round_num=1, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=2, agent_id=agent1.id, cooperates=False),
               Action(game_id=game3.id, round_num=2, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=3, agent_id=agent1.id, cooperates=False),
               Action(game_id=game3.id, round_num=3, agent_id=agent4.id, cooperates=False),
               Action(game_id=game3.id, round_num=4, agent_id=agent1.id, cooperates=False),
               Action(game_id=game3.id, round_num=4, agent_id=agent4.id, cooperates=False),
               Action(game_id=game4.id, round_num=1, agent_id=agent2.id, cooperates=True),
               Action(game_id=game4.id, round_num=1, agent_id=agent3.id, cooperates=True),
               Action(game_id=game4.id, round_num=2, agent_id=agent2.id, cooperates=True),
               Action(game_id=game4.id, round_num=2, agent_id=agent3.id, cooperates=True),
               Action(game_id=game4.id, round_num=3, agent_id=agent2.id, cooperates=True),
               Action(game_id=game4.id, round_num=3, agent_id=agent3.id, cooperates=True),
               Action(game_id=game4.id, round_num=4, agent_id=agent2.id, cooperates=True),
               Action(game_id=game4.id, round_num=4, agent_id=agent3.id, cooperates=True),
               Action(game_id=game5.id, round_num=1, agent_id=agent2.id, cooperates=True),
               Action(game_id=game5.id, round_num=1, agent_id=agent4.id, cooperates=False),
               Action(game_id=game5.id, round_num=2, agent_id=agent2.id, cooperates=False),
               Action(game_id=game5.id, round_num=2, agent_id=agent4.id, cooperates=False),
               Action(game_id=game5.id, round_num=3, agent_id=agent2.id, cooperates=False),
               Action(game_id=game5.id, round_num=3, agent_id=agent4.id, cooperates=False),
               Action(game_id=game5.id, round_num=4, agent_id=agent2.id, cooperates=False),
               Action(game_id=game5.id, round_num=4, agent_id=agent4.id, cooperates=False),
               Action(game_id=game6.id, round_num=1, agent_id=agent3.id, cooperates=True),
               Action(game_id=game6.id, round_num=1, agent_id=agent4.id, cooperates=False),
               Action(game_id=game6.id, round_num=2, agent_id=agent3.id, cooperates=True),
               Action(game_id=game6.id, round_num=2, agent_id=agent4.id, cooperates=False),
               Action(game_id=game6.id, round_num=3, agent_id=agent3.id, cooperates=True),
               Action(game_id=game6.id, round_num=3, agent_id=agent4.id, cooperates=False),
               Action(game_id=game6.id, round_num=4, agent_id=agent3.id, cooperates=True),
               Action(game_id=game6.id, round_num=4, agent_id=agent4.id, cooperates=False)]
    db.session.add_all(agent_game_associations)
    db.session.add_all(actions)
    db.session.commit()











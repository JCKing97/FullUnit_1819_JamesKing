/** <module> This file handles the management of strategies in the system
 * @author James King
 */

/**
 * strategy(+Name:string, +Description:string, +Options:list) is nondet
 *
 * A strategy in the system.
 *
 * @arg Name The name of the strategy
 * @arg Description A description of the strategy
 * @arg Name The options for the strategy e.g. trusting/distrusting
 */

strategy( "Cooperator" , "Cooperates every time", []).
strategy( "Defector" , "Defects every time", []).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip", ["trusting"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip", ["distrusting"]).



/**
 * find_strategies(--Strategies:dict) is nondet
 *
 * Get all the strategies in teh system and format them into a dictionary.
 *
 * @arg Strategies The strategies currently stored in the system formatted into a dict
 */

find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc, options: Options}, strategy(Name, Desc, Options), Strategies).
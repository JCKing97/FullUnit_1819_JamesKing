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
 * @arg Name The options for the strategy e.g. trusting/distrusting, lazy/proactive
 */

strategy( "Cooperator" , "Cooperates every time, does not bother to actively gossip", ["lazy"]).
strategy( "Cooperator" , "Cooperates every time, actively gossips and shares information", ["promote_self"]).
strategy( "Defector" , "Defects every time, actively gossips and shares information", ["spread_positive"]).
strategy( "Defector" , "Defects every time, does not bother to actively gossip", ["lazy"]).
strategy( "Defector" , "Defects every time, actively gossips and shares information", ["promote_self"]).
strategy( "Defector" , "Defects every time, actively gossips and shares information", ["spread_negative"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, does not actively gossip", ["trusting", "lazy"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively shares gossip with other agents", ["trusting", "proactive"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, does not actively gossip", ["distrusting", "lazy"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively shares gossip with other agents", ["distrusting", "proactive"]).


/**
 * find_strategies(--Strategies:dict) is nondet
 *
 * Get all the strategies in teh system and format them into a dictionary.
 *
 * @arg Strategies The strategies currently stored in the system formatted into a dict
 */

find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc, options: Options}, strategy(Name, Desc, Options), Strategies).
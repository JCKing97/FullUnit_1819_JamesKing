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
strategy( "Cooperator" , "Cooperates every time, actively gossips and promotes positive information on self", ["promote_self"]).
strategy( "Cooperator" , "Cooperates every time, actively gossips and promotes positive information on any random agent", ["spread_positive"]).
strategy( "Defector" , "Defects every time, does not bother to actively gossip", ["lazy"]).
strategy( "Defector" , "Defects every time, actively gossips and promotes positive information about self", ["promote_self"]).
strategy( "Defector" , "Defects every time, actively gossips and spreads negative information about others", ["spread_negative"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, does not actively gossip", ["trusting", "lazy"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively promotes own image with gossip", ["trusting", "promote_self"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively promotes positive image of good agents", ["trusting", "spread_accurate_positive"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively promotes negative image of bad agents", ["trusting", "spread_accurate_negative"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, does not actively gossip", ["distrusting", "lazy"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively promotes own image with gossip", ["distrusting", "promote_self"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively promotes positive image of good agents", ["distrusting", "spread_accurate_positive"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively promotes negative image of bad agents", ["distrusting", "spread_accurate_negative"]).
strategy( "Random", "Randomly selects from actions it is capable of at each timepoint", []).


/**
 * find_strategies(--Strategies:dict) is nondet
 *
 * Get all the strategies in the system and format them into a dictionary.
 *
 * @arg Strategies The strategies currently stored in the system formatted into a dict
 */

find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc, options: Options}, strategy(Name, Desc, Options), Strategies).
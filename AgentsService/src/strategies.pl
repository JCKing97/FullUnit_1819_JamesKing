/*--------------------------------------
Author:		James King
Title:		strategies.pl
Created:	4th Nov 2018
Desc:		Contains the logic related to agents strategies
--------------------------------------*/

strategy( "Cooperator" , "Cooperates every time", []).
strategy( "Defector" , "Defects every time", []).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip", ["trusting"]).
strategy( "Standing Discriminator", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip", ["distrusting"]).


find_strategies(Strategies):-
	findall(strategy{name: Name, description: Desc, options: Options}, strategy(Name, Desc, Options), Strategies).
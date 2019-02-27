/** <module> This file handles the management of strategies in the system
 * @author James King
 */

:- use_module(library(dicts)).
:- use_module(library(lists)).

/**
 * strategy(+DonorStrategy:string, +NonDonorStrategy:string, +TrustModel:string +Description:string, +Options:list) is nondet
 *
 * A strategy in the system.
 *
 * @arg Name The name of the strategy
 * @arg Description A description of the strategy
 * @arg Name The options for the strategy e.g. trusting/distrusting, lazy/proactive
 */

strategy( "Cooperator" , "Lazy", "Void", "Cooperates every time, does not bother to actively gossip", []).
strategy( "Cooperator" , "Promote Self", "Void", "Cooperates every time, actively gossips and promotes positive information on self", []).
strategy( "Cooperator" , "Spread Positive", "Void", "Cooperates every time, actively gossips and promotes positive information on any random agent", []).
strategy( "Defector" , "Lazy", "Void", "Defects every time, does not bother to actively gossip", []).
strategy( "Defector" , "Promote Self", "Void", "Defects every time, actively gossips and promotes positive information about self", []).
strategy( "Defector" , "Spread Negative", "Void", "Defects every time, actively gossips and spreads negative information about others", []).
strategy( "Standing Discriminator", "Lazy", "Trusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, does not actively gossip", []).
strategy( "Standing Discriminator", "Promote Self", "Trusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively promotes own image with gossip", []).
strategy( "Standing Discriminator", "Spread Accurate Positive", "Trusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively promotes positive image of good agents", []).
strategy( "Standing Discriminator", "Spread Accurate Negative", "Trusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Trusts other agents gossip, actively promotes negative image of bad agents", []).
strategy( "Standing Discriminator", "Lazy", "Distrusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, does not actively gossip", []).
strategy( "Standing Discriminator", "Promote Self", "Distrusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively promotes own image with gossip", []).
strategy( "Standing Discriminator", "Spread Accurate Positive", "Distrusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively promotes positive image of good agents", []).
strategy( "Standing Discriminator", "Spread Accurate Negative", "Distrusting", "Considers every other agent to start on a good standing, if they observe a defection towards an agent with good standing the donor that defected is given a bad standing. Cooperates with agents they deem to have good standing, defects against those with bad standing. Doesn't trust other agents gossip, actively promotes negative image of bad agents", []).
strategy( "Random", "Void", "Void", "Randomly selects from actions it is capable of at each timepoint", []).
strategy( "Image Scoring Discriminator", NonDonorStrategy, TrustModel, Description, [K]):-
	between(-2, 2, K),
	image_score_possible_options(NonDonorStrategy, TrustModel, NonDonorStrateTrustModelOptionsDescription),
	format(string(Description), "Holds a value for each player starting on 0, when interacting if the agent holds a value of greater than or equal to K=~d for the recipient they cooperate, else they defect. ~w", [K, NonDonorStrateTrustModelOptionsDescription]).

image_score_possible_options("Lazy", "Trusting", "Agent never spreads gossip, but trusts others gossip (if the gossiper is of a value greater than K)").
image_score_possible_options("Lazy", "Distrusting", "Agent never spreads gossip and never trusts gossip").
image_score_possible_options("Spread Accurate Negative", "Trusting",  "Agent spreads negative gossip about those it distrusts (value < K) and trusts other trusted agents gossip").
image_score_possible_options("Spread Accurate Negative", "Distrusting", "Agent spreads negative gossip about those it distrusts (value < K) but never trusts others gossip").
image_score_possible_options("Spread Accurate Positive", "Trusting", "Agent spreads positive gossip about those it trusts (value >= K) and trusts others trusted agents gossip").
image_score_possible_options("Spread Accurate Positive", "Distrusting", "Agent spreads positive gossip about those it trusts (value >= K), but never trusts others gossip").
image_score_possible_options("Promote Self", "Trusting", "Agent spreads positive gossip to promote themself, trusts gossip from agents it trusts (value >= K)").
image_score_possible_options("Promote Self", "Distrusting", "Agent spreads positive gossip to promote themself, doesn't trust other agents gossip").



/**
 * find_strategies(--Strategies:dict) is nondet
 *
 * Get all the strategies in the system and format them into a dictionary.
 *
 * @arg Strategies The strategies currently stored in the system formatted into a dict
 */

find_strategies(Strategies):-
	findall(strategy{donor_strategy: DonorStrategy, non_donor_strategy: NonDonorStrategy, trust_model: TrustModel, description: Desc, options: Options}, strategy(DonorStrategy, NonDonorStrategy, TrustModel, Desc, Options), Strategies).
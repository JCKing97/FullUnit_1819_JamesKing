/** <module> This file handles getting commitments to actions from agents, and surrounding logic.
 * @author James King
 */

:- use_module(library(lists)).
:- use_module(library(random)).

?- ['./agents'].
?- ['./communities'].
?- ['./utilities'].
% Compile and set up mvfcec
?-['./mvfcec/src/lib/utilities'].
?-['./mvfcec/src/compiler/basic_V1.0'].
?-['./mvfcec/src/lib/activity_recognition_lifecycles'].
:- (dynamic observed_at/2, action_commitment/5).
:- discontiguous agent_action/6.

/**
 * capabilities(++Timepoint:int, ++CommunityID:int, ++GenerationID:int, ++AgentID:int, -Capabilities:list) is nondet
 *
 * Get the actions the agent with AgentID believes that it is capable of at the given timepoint.
 *
 * @arg Timepoint The timepoint at which to get the action commitment at
 * @arg CommunityID The community the agent belongs to
 * @arg GenerationID The generation of the community the agent belongs to
 * @arg AgentID The id of the agent
 * @arg Capabilities A list of actions an agent believes it is capable of
 */

get_capabilities(Timepoint, CommunityID, GenerationID, AgentID, Capabilities):-
 	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(Action, capable(Timepoint, CommunityID, GenerationID, AgentID, Action), Capabilities).


/**
 * capable(++Timepoint:int, ++CommunityID:int, ++GenerationID:int, ++AgentID:int, -Action:term) is nondet
 *
 * Unifies with an action the given agent is capable of at the given timepoint.
 *
 * @arg Timepoint The timepoint at which to get the action commitment at
 * @arg CommunityID The community the agent belongs to
 * @arg GenerationID The generation of the community the agent belongs to
 * @arg AgentID The id of the agent
 * @arg Action An action the agent is capable of
 */

% For when the agent is a donor
capable(Timepoint, CommunityID, GenerationID, AgentID, Action):-
	observed_at(interaction(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID)), Timepoint),
	( Action = action{type: action, value: defect, recipient: RecipientID, reason: _} ;
	  Action = action{type: action, value: cooperate, recipient: RecipientID, reason: _}  ).
% For when the agent is not a donor
capable(Timepoint, CommunityID, GenerationID, AgentID, Action):-
	\+observed_at(interaction(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), _)), Timepoint),
	Action = action{type: idle, reason: _}.
capable(Timepoint, CommunityID, GenerationID, AgentID, Action):-
	\+observed_at(interaction(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), _)), Timepoint),
	agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	AboutID \== RecipientID,
	( Action = action{type: gossip, value: positive, about: AboutID, recipient: RecipientID, reason:_} ;
	  Action = action{type: gossip, value: negative, about: AboutID, recipient: RecipientID, reason: _} ).

/**
 * get_action(++Timepoint:int, ++CommunityID:int, ++GenerationID:int, ++AgentID:int, -Success:string, -Action:term) is nondet
 *
 * Get the action for the given agent and check whether the agent is capable of it.
 *
 * @arg Timepoint The timepoint at which to get the action commitment at
 * @arg CommunityID The community the agent belongs to
 * @arg GenerationID The generation of the community the agent belongs to
 * @arg AgentID The id of the agent
 * @arg Success true if getting an action was successful, error message otherwise
 * @arg Action The action that the agent has decided upon
 */

get_action(Timepoint, CommunityID, GenerationID, AgentID, "Agent has already committed to an action at this timepoint", Action):-
	action_commitment(Timepoint, CommunityID, GenerationID, AgentID, Action), !.
get_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	agent_action(Timepoint, CommunityID, GenerationID, AgentID, AgentSuccess, Action), !,
	( Action == false -> Success = AgentSuccess ;
		(capable(Timepoint, CommunityID, GenerationID, AgentID, Action) ->
			( Success = AgentSuccess, assert(action_commitment(Timepoint, CommunityID, GenerationID, AgentID, Action)) ) ;
			Success = "Agent attempted to pick a non-permitted action"
		)
	).
get_action(_, _, _, _, "Failed to find an action for this agent", false).


/**
 * agent_action(++Timepoint:int, ++CommunityID:int, ++GenerationID:int, ++AgentID:int, -Success:atom, -Action:dict) is nondet
 *
 * Get an agents commitment to an action, responds unsuccessful if there is no such community, generation of the community or agent belonging to the generation of the community passed.
 *
 * @arg Timepoint The timepoint at which to get the action commitment at
 * @arg CommunityID The community the agent belongs to
 * @arg GenerationID The generation of the community the agent belongs to
 * @arg AgentID The id of the agent
 * @arg Success An output argument whether getting the action was successful (becomes true) or not (becomes error message) 
 * @arg Action An output argument which is a dictionary representing the action commitment made
 */

/*----------------------------------------------------------------------------------------
---------------------------------------- Get Actions -------------------------------------
----------------------------------------------------------------------------------------*/

/*-----------------------------
----------- Defector ----------
-----------------------------*/

% If the agent is a donor this turn
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true,
 action{type: action, value: defect, recipient: RecipientID, reason: "To protect my interests, and not incur cooperation costs"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: defect, recipient: RecipientID, reason: _}), !.	
% Auto to idle if not a donor and using the lazy strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle, reason: "I only act when I have to"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle, reason: _}), !.	
% Spread negative information randomly if using the spread_negative strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", "Spread Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(action{type: gossip, value: negative, about: AboutID, recipient: RecipientID},
	 (capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: negative, about: AboutID, recipient: RecipientID, reason: _}),
	  AgentID\==AboutID, AgentID\==RecipientID), SpreadNegativeCapabilities),
	random_element(SpreadNegativeCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I spread negative gossip to deceive others and add fake information to the society", Action), !.	
% Spread positive information about self if using the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID, reason: _}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I spread positive gossip to deceive others and encourage them to cooperate with me", Action), !.

/*-----------------------------
---------- Cooperator ---------
-----------------------------*/

% If the agent is a donor this turn
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true,
 action{type:action, value: cooperate, recipient: RecipientID, reason: "I naively cooperate with everyone out of pure altruism"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID, reason: _}), !.
% Auto to idle if not a donor and lazy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle, reason: "I only act when I have to"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle, reason: _}), !.	
% Auto to spreading positive information about self if not a donor and using the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID, reason: _},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID, reason: _}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I spread positive gossip about myself to encourage others to cooperate with me", Action), !.
% Auto to spreading positive information about random if not a donor and using the spread_positive strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", "Spread Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(action{type: gossip, value: positive, about: AboutID, recipient: RecipientID},
	 (capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AboutID, recipient: RecipientID, reason: _}),
	  AgentID\==RecipientID), SpreadPositiveCapabilities),
	random_element(SpreadPositiveCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I naively spread positive gossip about others to encourage cooperation in the system", Action), !.

/*-----------------------------
------ Standing Strategy ------
-----------------------------*/

% If the agent is a donor this turn and holds the recipient in good standing: cooperate
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, 
	action{type: action, value: cooperate, recipient: RecipientID, reason: "I am cooperating because I believe the recipient to have a good standing"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID, reason: _}),
	( 
		\+holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=bad, Timepoint+1) ;
	  	holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=good, Timepoint+1)
	), !.
% If the agent is a donor this turn and holds the recipient in bad standing: defect
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true,
 action{type: action, value: defect, recipient: RecipientID, reason: "I am defecting because I believe the recipient to have a bad standing"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID, reason: _}),
	holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=bad, Timepoint+1), !.
% If the agent is not a donor in this turn and follows the lazy strategy be idle
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle, reason: "I only act when I have to"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle, reason: _}), !.
% If the agent is a not a donor and follows the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID, reason: _}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I actively promote myself as I wish to encourage others to cooperate with me", Action), !.
% If the agent is not a donor and follows the spread_accurate_positive strategy, spread positive gossip about good agents or default to idle
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true,
 action{type: gossip, value: positive, about: GoodAgent, recipient: RecipientID,
  reason: "I am spreading positive gossip because I believe the recipient and the agent it is about are good"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(ID, 
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), ID),
			\+holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
				agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), ID))=bad, Timepoint+1),
			ID \== AgentID
		),
	GoodAgents),
	length(GoodAgents, Len), Len >= 2, !,
	random_element(GoodAgents, GoodAgent),
	delete(GoodAgents, GoodAgent, OtherGoodAgents),
	random_element(OtherGoodAgents, RecipientID).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle,
 reason: "I know no two good agents to be the recipient and the target of positive gossip"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}), !.


% If the agent is not a donor and follows the spread_accurate_negative strategy, spread negative gossip about bad agents or default to idle
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: negative, about: BadAgent, recipient: RecipientID,
 reason: "I believe the agent this gossip is about to be of bad standing, so I am spreading it to those I believe are of good standing"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(BadAgentID,
		holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID))=bad, Timepoint+1),
		BadAgents
	),
	findall(GoodAgentID, 
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID),
			\+holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
				agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID))=bad, Timepoint+1),
			GoodAgentID \== AgentID
		),
	GoodAgents),
	is_empty(BadAgents, false),
	is_empty(GoodAgents, false),
	random_element(BadAgents, BadAgent),random_element(GoodAgents, RecipientID), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle, reason: "I know of no agents with bad standing to warn others about"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(BadAgentID,
		holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID))=bad, Timepoint+1),
		BadAgents
	),is_empty(BadAgents, true), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle,
 reason: "I know of no agents with good standing to warn of the agents with bad standing"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(GoodAgentID, 
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID),
			\+holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
				agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID))=bad, Timepoint+1),
			GoodAgentID \== AgentID
		),
	GoodAgents),is_empty(GoodAgents, true), !.

/*-----------------------------
----------- Random ------------
-----------------------------*/

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Random", "Random", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	get_capabilities(Timepoint, CommunityID, GenerationID, AgentID, Capabilities),
	random_element(Capabilities, RandomAction),
	put_dict(reason, RandomAction, "I randomly select any action I am capable of", Action), !.

/*-------------------------------------
------ Image Score Discriminator ------
-------------------------------------*/

% If the agent is a donor 
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", _, _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID, reason: _}),
	(
		holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
							 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=ImageScore, Timepoint) ->
			(
				K > ImageScore -> (Action = action{type: action, value: defect, recipient: RecipientID, reason: "I am defecting as I do not believe the recipient's image is worthy of cooperation"}, !) ; 
								  (Action = action{type: action, value: cooperate, recipient: RecipientID, reason: "I am cooperating as I believe the recipient's image is worthy of cooperation"}, !)
			) ;
			(
				K > 0 -> (Action = action{type: action, value: defect, recipient: RecipientID, reason: "I am defecting as I do not know of the recipient's image so I cannot trust them"}, !) ; 
						 (Action = action{type: action, value: cooperate, recipient: RecipientID, reason: "I am cooperating as I do not know the recipient's image, but I think should give them a chance"}, !)
			)
	), Success = true, !.

% If the agent is not a donor and uses the lazy strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle, reason: "I only act when I have to"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Lazy", _,  _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle, reason: _}), !.

% If the agent is not a donor and uses the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID, reason: _}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I promote myself because I want to encourage others to cooperate with me", Action), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: positive, about: GoodAgent, recipient: RecipientID,
	reason: "I believe the agent this gossip is about is good and want to spread that belief to other agents I believe are good"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Positive", _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(GoodAgentID,
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID),
			(
				holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID))=ImageScore, Timepoint+1) ->
					ImageScore >= K ;
					0 >= K
			),
			GoodAgentID \== AgentID
		),
		GoodAgents
	),
	length(GoodAgents, Len), Len >= 2, !,
	random_element(GoodAgents, GoodAgent),
	delete(GoodAgents, GoodAgent, OtherGoodAgents),
	random_element(OtherGoodAgents, RecipientID).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle,
 reason: "I know no two good agents to be the recipient and the target of positive gossip"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: negative, about: BadAgent, recipient: RecipientID,
 reason: "I believe the agent the gossip is about to have a bad image, I am warning those I believe are good"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Negative", _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}),
	findall(BadAgentID,
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID),
			( 
				holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID))=ImageScore, Timepoint+1) ->
						ImageScore < K ;
						0 < K
			),
			BadAgentID \== AgentID
		),
		BadAgents
	),
	findall(GoodAgentID,
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID),
			(
				holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID))=ImageScore, Timepoint+1) ->
					ImageScore >= K ;
					0 >= K
			),
			GoodAgentID \== AgentID
		),
		GoodAgents
	),
	is_empty(BadAgents, false), is_empty(GoodAgents, false),
	random_element(BadAgents, BadAgent),random_element(GoodAgents, RecipientID), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle,
 reason: "I know of no agents with good standing to warn of the agents with bad standing"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Negative", _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}), 
	findall(GoodAgentID,
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID),
			(
				holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), GoodAgentID))=ImageScore, Timepoint+1) ->
					ImageScore >= K ;
					0 >= K
			),
			GoodAgentID \== AgentID
		),
		GoodAgents
	), is_empty(GoodAgents, true), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle,
 reason: "I know of no agents with bad standing to warn others about"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Negative", _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _, reason: _}), 
	findall(BadAgentID,
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID),
			( 
				holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID))=ImageScore, Timepoint+1) ->
						ImageScore < K ;
						0 < K
			),
			BadAgentID \== AgentID
		),
		BadAgents
	), is_empty(BadAgents, true), !.

/*--------------------------------------
-------- Veritability Discerner --------
--------------------------------------*/

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: action, value: Action, recipient: RecipientID, reason: Reason}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", _, _, _, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID, reason: _}),
	trusted(agent(strategy("Veritability Discerner", _, _, _, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID), Timepoint),
	Action=cooperate, !,
	(holds_at(percept_count(agent(strategy("Veritability Discerner", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=_, Timepoint) ->
			Reason="The recipient has generally acted in a good manner" ;
			Reason="I don't know anything about the recipient, but I am giving them a chance"
	).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: action, value: Action, recipient: RecipientID, reason: Reason}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID, reason: _}),
	Action=defect, !,
	( holds_at(percept_count(agent(strategy("Veritability Discerner", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=_, Timepoint) ->
			Reason="The recipient's actions haven't been worthy of cooperation" ;
		 	Reason="I don't know anything about the recipient, so I will protect myself and defect"
	).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle, reason: "I only act when I have to"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle, reason: _}), !.
	
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID, reason: _}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, SelectedAction),
	put_dict(reason, SelectedAction, "I promote myself because I want to encourage others to cooperate with me", Action), !.

filter_trusted_agents([], _, [], [], _).
filter_trusted_agents([Agent|OtherAgents], Perceiver, TrustedAgents, UntrustedAgents, Timepoint):-
	filter_trusted_agents(OtherAgents, Perceiver, OtherTrustedAgents, OtherUntrustedAgents, Timepoint),
	( trusted(Perceiver, Agent, Timepoint) ->
		( append([Agent], OtherTrustedAgents, TrustedAgents), UntrustedAgents=OtherUntrustedAgents );
		( append([Agent], OtherUntrustedAgents, UntrustedAgents), TrustedAgents=OtherTrustedAgents )).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: positive, about: TrustedAgentID, recipient: RecipientID,
	reason: "I believe the agent this gossip is about is trustworthy and want to spread that belief to other agents I believe are trustworthy"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Spread Positive Trusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
		(
			agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
			OtherAgentID\==AgentID
		),
		Agents
	),
	filter_trusted_agents(Agents, 
		agent(strategy("Veritability Discerner", "Spread Positive Trusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		TrustedAgents, _, Timepoint),
	length(TrustedAgents, Len), Len >= 2, !,
	random_element(TrustedAgents, TrustedAgent),
	TrustedAgent=agent(_, _, _, TrustedAgentID),
	delete(TrustedAgents, TrustedAgent, OtherTrustedAgents),
	random_element(OtherTrustedAgents, Recipient),
	Recipient=agent(_, _, _, RecipientID).

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle,
	reason: "I know no two trustworthy agents to spread positive gossip to and about."}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle, reason: _}),
	agent(strategy("Veritability Discerner", "Spread Positive Trusted", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: negative, about: UnrustedAgentID, recipient: TrustedAgentID,
	reason: "I believe the agent this gossip is about is untrustworthy and want to warn agents I believe are trustworthy"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
		(
			agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
			OtherAgentID\==AgentID
		),
		Agents
	),
	filter_trusted_agents(Agents, 
		agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		TrustedAgents, UntrustedAgents, Timepoint),
	is_empty(TrustedAgents, false),
	is_empty(UntrustedAgents, false), !,
	random_element(TrustedAgents, TrustedAgent),
	TrustedAgent=agent(_, _, _, TrustedAgentID),
	random_element(UntrustedAgents, UntrustedAgent), 
	UntrustedAgent=agent(_, _, _, UnrustedAgentID), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle,
	reason: "I do not know any trustworthy or untrustworthy agents to gossip to or about"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
		(
			agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
			OtherAgentID\==AgentID
		),
		Agents
	),
	filter_trusted_agents(Agents, 
		agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		TrustedAgents, UntrustedAgents, Timepoint),
	is_empty(TrustedAgents, true),
	is_empty(UntrustedAgents, true), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle,
	reason: "I do not know any trustworthy agents to warn about untrustworthy agents"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
		(
			agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
			OtherAgentID\==AgentID
		),
		Agents
	),
	filter_trusted_agents(Agents, 
		agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		TrustedAgents, _, Timepoint),
	is_empty(TrustedAgents, true), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: idle,
	reason: "I do not know any untrustworthy agents to warn trustworthy agents about"}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	findall(agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
		(
			agent(Strat, community(CommunityID), generation(community(CommunityID), GenerationID), OtherAgentID),
			OtherAgentID\==AgentID
		),
		Agents
	),
	filter_trusted_agents(Agents, 
		agent(strategy("Veritability Discerner", "Spread Negative Untrusted", TrustModel, Desc, [K]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		_, UntrustedAgents, Timepoint),
	is_empty(UntrustedAgents, true), !.


	

/*-----------------------------
----------- Failure -----------
-----------------------------*/

agent_action(_, CommunityID, _, _, Success, Action):-
	\+community(CommunityID),
	Success = 'No such community', Action = false, !.
agent_action(_, CommunityID, GenerationID, _, Success, Action):-
	\+generation(community(CommunityID), GenerationID),
	Success = 'No such generation for this community', Action = false, !.
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	\+agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Success = 'No such player for this generation of this community', Action = false, !.

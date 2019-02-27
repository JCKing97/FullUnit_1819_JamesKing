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
	( Action = action{type: action, value: defect, recipient: RecipientID} ;
	  Action = action{type: action, value: cooperate, recipient: RecipientID}  ).
% For when the agent is not a donor
capable(Timepoint, CommunityID, GenerationID, AgentID, Action):-
	\+observed_at(interaction(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), _)), Timepoint),
	Action = action{type: idle}.
capable(Timepoint, CommunityID, GenerationID, AgentID, Action):-
	\+observed_at(interaction(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), _)), Timepoint),
	agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AboutID),
	agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID),
	AboutID \== RecipientID,
	( Action = action{type: gossip, value: positive, about: AboutID, recipient: RecipientID} ;
	  Action = action{type: gossip, value: negative, about: AboutID, recipient: RecipientID} ).

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

get_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	action_commitment(Timepoint, CommunityID, GenerationID, AgentID, Action), !,
	Success = "Agent has already committed to an action at this timepoint".
get_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	agent_action(Timepoint, CommunityID, GenerationID, AgentID, AgentSuccess, Action), !,
	( Action == false -> Success = AgentSuccess ;
		(capable(Timepoint, CommunityID, GenerationID, AgentID, Action) ->
			( Success = AgentSuccess, assert(action_commitment(Timepoint, CommunityID, GenerationID, AgentID, Action)) ) ;
			Success = "Agent attempted to pick a non-permitted action"
		)
	).
get_action(_, _, _, _, Success, Action):-
	Success = "Failed to find an action for this agent",
	Action = false.


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
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: defect, recipient: RecipientID}),
	Success = true, Action = action{type: action, value: defect, recipient: RecipientID}, !.	
% Auto to idle if not a donor and using the lazy strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle}),
	Success = true, Action = action{type: idle}, !.	
% Spread negative information randomly if using the spread_negative strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", "Spread Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: negative, about: AboutID, recipient: RecipientID},
	 (capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: negative, about: AboutID, recipient: RecipientID}),
	  AgentID\==AboutID, AgentID\==RecipientID), SpreadNegativeCapabilities),
	random_element(SpreadNegativeCapabilities, Action),
	Success = true, !.	
% Spread positive information about self if using the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, Action),
	Success = true, !.	

/*-----------------------------
---------- Cooperator ---------
-----------------------------*/

% If the agent is a donor this turn
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID}),
	Success = true, Action = action{type:action, value: cooperate, recipient: RecipientID}, !.
% Auto to idle if not a donor and lazy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle}),
	Success = true, Action = action{type: idle}, !.	
% Auto to spreading positive information about self if not a donor and using the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, Action),
	Success = true, !.
% Auto to spreading positive information about random if not a donor and using the spread_positive strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", "Spread Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: positive, about: AboutID, recipient: RecipientID},
	 (capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AboutID, recipient: RecipientID}),
	  AgentID\==RecipientID), SpreadPositiveCapabilities),
	random_element(SpreadPositiveCapabilities, Action),
	Success = true, !.

/*-----------------------------
------ Standing Strategy ------
-----------------------------*/

% If the agent is a donor this turn and holds the recipient in good standing: cooperate
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID}),
	( 
		\+holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=bad, Timepoint+1) ;
	  	holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=good, Timepoint+1)
	),
	Success = true, Action = action{type: action, value: cooperate, recipient: RecipientID}, !.
% If the agent is a donor this turn and holds the recipient in bad standing: defect
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID}),
	holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=bad, Timepoint+1),
	Success = true, Action = action{type: action, value: defect, recipient: RecipientID}, !.
% If the agent is not a donor in this turn and follows the lazy strategy be idle
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Lazy", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle}),
	Success = true, Action = action{type: idle}, !.
% If the agent is a not a donor and follows the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, Action),
	Success = true, !.
% If the agent is not a donor and follows the spread_accurate_positive strategy, spread positive gossip about good agents or default to idle
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(ID, 
		(
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), ID),
			\+holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
				agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), ID))=bad, Timepoint+1),
			ID \== AgentID
		),
	GoodAgents),
	(is_empty(GoodAgents, true) -> 
		Action = action{type: idle}, ! ;
		(
			random_element(GoodAgents, GoodAgent),
			findall(ID, 
				(
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), ID),
					ID \== GoodAgent,
					ID \== AgentID
				),
			Agents),
			random_element(Agents, RecipientID),
			Action = action{type: gossip, value: positive, about: GoodAgent, recipient: RecipientID}, !
		)
	), !.
% If the agent is not a donor and follows the spread_accurate_negative strategy, spread negative gossip about bad agents or default to idle
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", "Spread Accurate Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(BadAgentID,
		holds_at(standing(agent(strategy("Standing Discriminator", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), BadAgentID))=bad, Timepoint+1),
		BadAgents
	),
	(is_empty(BadAgents, true) -> 
		Action = action{type: idle}, ! ;
		(
			random_element(BadAgents, BadAgent),
			findall(ID, 
				(
					agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), ID),
					ID \== BadAgent,
					ID \== AgentID
				),
			Agents),
			random_element(Agents, RecipientID),
			Action = action{type: gossip, value: negative, about: BadAgent, recipient: RecipientID}, !
		)
	), !.

/*-----------------------------
----------- Random ------------
-----------------------------*/

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Random", _, _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	get_capabilities(Timepoint, CommunityID, GenerationID, AgentID, Capabilities),
	random_element(Capabilities, Action), !.

/*-------------------------------------
------ Image Score Discriminator ------
-------------------------------------*/

% If the agent is a donor 
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", _, _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: cooperate, recipient: RecipientID}),
	(
		holds_at(image_score(agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
							 agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=ImageScore, Timepoint) ->
			(
				K > ImageScore -> (Action = action{type: action, value: defect, recipient: RecipientID}, !) ; 
								  (Action = action{type: action, value: cooperate, recipient: RecipientID}, !)
			) ;
			(
				K > 0 -> (Action = action{type: action, value: defect, recipient: RecipientID}, !) ; 
						 (Action = action{type: action, value: cooperate, recipient: RecipientID}, !)
			)
	), Success = true, !.

% If the agent is not a donor and uses the lazy strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Lazy", _,  _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle}),
	Action = action{type: idle},
	Success = true, !.

% If the agent is not a donor and uses the promote_self strategy
agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Promote Self", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, Action), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: positive, about: GoodAgent, recipient: RecipientID}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Positive", _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
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

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Positive", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}), !.

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type: gossip, value: negative, about: BadAgent, recipient: RecipientID}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Negative", _, _, [K|_]), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
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

agent_action(Timepoint, CommunityID, GenerationID, AgentID, true, action{type:idle}):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Image Scoring Discriminator", "Spread Accurate Negative", _, _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}), !.


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

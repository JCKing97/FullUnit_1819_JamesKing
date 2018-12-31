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
dialect(swi).
:- (dynamic observed_at/2).
input_format(observed_at(E, T), E, T).

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
 * capable(++Timepoint:int, ++CommunityID:int, ++GenerationID:int, ++AgentID:int, ++Action:term) is nondet
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
	agent_action(Timepoint, CommunityID, GenerationID, AgentID, AgentSuccess, Action), !,
	( capable(Timepoint, CommunityID, GenerationID, AgentID, Action) ->
		Success = AgentSuccess ;
		Success = "Agent attempted to pick a non-permitted action"
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
	agent(strategy("Defector", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: defect, recipient: RecipientID}),
	Success = true, Action = action{type: action, value: defect, recipient: RecipientID}, !.	
% Auto to idle if not a donor and using the lazy strategy
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", _, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	member("lazy", Options),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle}),
	Success = true, Action = action{type: idle}, !.	
% Spread negative information randomly if using the spread_negative strategy
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", _, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	member("spread_negative", Options),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: negative, about: AboutID, recipient: RecipientID},
	 (capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: negative, about: AboutID, recipient: RecipientID}),
	  AgentID\==AboutID, AgentID\==RecipientID), SpreadNegativeCapabilities),
	random_element(SpreadNegativeCapabilities, Action),
	Success = true, !.	
% Spread positive information about self if using the promote_self strategy
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", _, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	member("promote_self", Options),
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
	agent(strategy("Cooperator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	holds_at(interaction_timepoints(agent(strategy("Cooperator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=InteractionTimepoints, Timepoint+1),
	member(Timepoint, InteractionTimepoints),
	Success = true, Action = action{type:action, value: cooperate, recipient: RecipientID}, !.
% Auto to idle if not a donor and lazy
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", _, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	member("lazy", Options),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: idle}),
	Success = true, Action = action{type: idle}, !.	
% Auto to spreading positive information about self if not a donor and using the promote_self strategy
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", _, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	member("promote_self", Options),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: positive, about: AgentID, recipient: RecipientID},
	 capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: positive, about: AgentID, recipient: RecipientID}),
	  SelfPromotionCapabilities),
	random_element(SelfPromotionCapabilities, Action),
	Success = true, !.	
% Auto to spreading positive information about random if not a donor and using the spread_positive strategy
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", _, Options), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	member("spread_positive", Options),
	\+capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: action, value: _, recipient: _}),
	findall(action{type: gossip, value: negative, about: AboutID, recipient: RecipientID},
	 (capable(Timepoint, CommunityID, GenerationID, AgentID, action{type: gossip, value: negative, about: AboutID, recipient: RecipientID}),
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
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	CheckTimepoint is Timepoint+1,
	holds_at(interaction_timepoints(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=InteractionTimepoints, CheckTimepoint),
	member(Timepoint, InteractionTimepoints),
	( 
		\+holds_at(standing(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=bad, Timepoint+1) ;
	  	holds_at(standing(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
			agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=good, Timepoint+1)
	),
	Success = true, Action = action{type: action, value: cooperate, recipient: RecipientID}, !.
% If the agent is a donor this turn and holds the recipient in bad standing: defect
agent_action(Timepoint, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	CheckTimepoint is Timepoint+1,
	holds_at(interaction_timepoints(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=InteractionTimepoints, CheckTimepoint),
	member(Timepoint, InteractionTimepoints),
	holds_at(standing(agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID), 
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=bad, Timepoint+1),
	Success = true, Action = action{type: action, value: defect, recipient: RecipientID}, !.
% If the agent is a donor this turn and holds the recipient in bad standing: defect
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Standing Discriminator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Success = true, Action = action{type: idle}, !.

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

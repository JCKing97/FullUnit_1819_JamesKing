/*--------------------------------------
Author:		James King
Title:		actions.pl
Created:	Nov 2018
Desc:		Contains the logic related to agents actions
--------------------------------------*/

:- use_module(library(http/http_log)).

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
	holds_at(interaction_timepoints(agent(strategy("Defector", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
		agent(_, community(CommunityID), generation(community(CommunityID), GenerationID), RecipientID))=InteractionTimepoints, Timepoint+1),
	member(Timepoint, InteractionTimepoints),
	Success = true, Action = action{type: action, value: defect, recipient: RecipientID}, !.	
% Auto to idle if not a donor
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Defector", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Success = true, Action = action{type: idle}, !.	

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
	Success = true, Action = action{type:action, value:cooperate, recipient: RecipientID}, !.
% Auto to idle if not a donor
agent_action(_, CommunityID, GenerationID, AgentID, Success, Action):-
	community(CommunityID),
	generation(community(CommunityID), GenerationID),
	agent(strategy("Cooperator", _, _), community(CommunityID), generation(community(CommunityID), GenerationID), AgentID),
	Success = true, Action = action{type: idle}, !.

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

log_list([A]):-
	http_log('~w~n', [A]).
log_list([A|B]):-
	http_log('~w, ', [A]),
	log_list(B).

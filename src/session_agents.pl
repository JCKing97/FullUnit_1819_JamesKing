/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          session_agents.pl
Created:        4th Oct 2018
Desc:           The file containing content creation for the session_agentss page and session_agent management
-------------------------------------------*/

% Define the module and the available predicates/non-terms from it
:- module(session_agents, [session_agent//1, session_agent_html//1, session_agents_page_content//0, get_session_agents_page_content//1]).

% Token modules
:- use_module(border).
:- use_module(agents).

% Create content for the session agents page
session_agents_page_content -->
	{
		findall([AgentName, AgentDescription], agents:agent(AgentName, AgentDescription), Agents)
	},
	html([
		h1("Session Agents"),
		p("Select agents for your session"),
		div(\select_agents_form),
		p("Don't know what an agent does? here's the description:"),
		div(\agents_display(Agents))
	]).
% The form for selecting a session agent
select_agents_form -->
	{
		http_link_to_id(session_agents, [], PostHREF)
	},
	html([
		form([action=PostHREF, method='POST'],[
			p([], [
				label([for=agent_selector], 'Select an agent:'),
				input([name=agent_selector, type=textarea])
			]),
			p([], input([name=submit, type=submit, value='Submit'], []))
		])
	]).
% The content to display given the Agents passed in
get_session_agents_page_content(Agents) -->
	html([
		h1("Selected Agents"),
		p("Here are the agents you selected"),
		div(\session_agents_display(Agents))
	]).
session_agents_display(Agents) -->
	{
		session_agent_display([], Agents, HTML)
	},
	HTML.
session_agent_html(AgentName) -->
	html([
		p("Agent Name: ~w"-AgentName)
	]).
session_agent_display(OldHTML, [Agent], NewHTML):-
	NewHTML = html([
		\OldHTML,
		\my_fancy_border(\session_agent_html(Agent))
	]).
session_agent_display(OldHTML, [Agent|NextAgents], NewHTML):-
	ParseDownHTML = html([
		\OldHTML,
		\my_fancy_border(\session_agent_html(Agent))
	]),
	session_agent_display(ParseDownHTML, NextAgents, NewHTML).

/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          agents.pl
Created:        4th Oct 2018
Desc:           The file containing content creation for the agents page and agent management
-------------------------------------------*/

:- module(agents, [agent//2, agent_html//1, get_agent_page_content//0, agents_display//1]).

:- use_module(border).

agent("ExampleName", "ExampleDescription").
get_agent_page_content -->
	{
		findall([AgentName,AgentDescription], agent(AgentName, AgentDescription), Agents)
	},
	html([
		h1("Agents"),
		p("Here is a list of agents in our knowledge base"),
		div(\agents_display(Agents))
	]).

agents_display(Agents) -->
	{
		agent_display([], Agents, HTML)
	},
	HTML.
agent_html([AgentName, AgentDescription]) -->
	html([
        	p("Agent Name: ~w"-AgentName),
        	p("Agent Description: ~w"-AgentDescription)
	]).
agent_display(OldHTML, [Agent], NewHTML):-
	NewHTML = html([
		\OldHTML,
		\my_fancy_border(\agent_html(Agent))
	]).
agent_display(OldHTML, [Agent|NextAgents], NewHTML):-
	ParseDownHTML = html([
		\OldHTML,
		\my_fancy_border(\agent_html(Agent))
	]),
	agent_display(ParseDownHTML, NextAgents, NewHTML).

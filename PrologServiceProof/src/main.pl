/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          main.pl
Created:        4th Oct 2018
Desc:           The main file of my proof of concept application for a Prolog web service
-------------------------------------------*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_session)).
:- use_module(library(lists)).
:- use_module(border).
:- use_module(home).
:- use_module(message_of_the_day).

:- http_handler(root(.), home_page, []).
:- http_handler(root(new_agent), new_agent, []).
:- http_handler(root(get_agents), get_agents, []).
:- http_handler(root(session_agents), session_agents, []).

server(Port):-
        http_server(http_dispatch, [port(Port)]).
        
home_page(Request):-
	member(method(get), Request), !,
	reply_html_page(
		proof_style,
		[title('Prolog Service - Home')],
		[\home_page_content]
	).
new_agent(Request):-
	member(method(get), Request), !,
	reply_html_page(
		proof_style,
		[title('Prolog Service - New Agent')],
		[\new_agent_page_content]
	).
new_agent(Request):-
	member(method(post), Request), !,
        http_read_data(Request, [agent_name=AgentName,agent_description=AgentDescription|_], []),
	assert(agent(AgentName, AgentDescription)),
	get_agents(Request).

get_agents(Request):-
	(member(method(post), Request) ; member(method(get), Request)),
	reply_html_page(
		proof_style,
		[title('Prolog Service - Agents')],
		[\get_agent_page_content]
	).

session_agents(Request):-
	member(method(get), Request), !,
	reply_html_page(
		proof_style,
		[title('Prolog Service - Select Agents')],
		[\session_agents_page_content]
	).
session_agents(Request):-
	member(method(post), Request), !,
	http_read_data(Request, [agent_selector=Agent|_], []),
	( agent(Agent, _) -> http_session_assert(session_agent(Agent)) ; http_session_assert(session_agent("No such agent")) ),
	get_session_agents(Request).
get_session_agents(Request):-
	( member(method(get), Request) ; member(method(post), Request) ),
	reply_html_page(
		proof_style,
		[title('Prolog Service - Session Agents')],
		[\get_session_agents_page_content(Request)]
	).

new_agent_page_content -->
	html([
		h1("New Agent"),
		p("Create new agents here"),
		\my_fancy_border(\new_agent_form)
	]).

new_agent_form -->
	{
		http_link_to_id(new_agent, [], PostHREF)
	},
	html([
		form([action=PostHREF, method='POST'],[
			p([], [
				label([for=agent_name], 'Agent Name:'),
				input([name=agent_name, type=textarea])
			]),
			p([], [
				label([for=agent_description], 'Agent Description:'),
				input([name=agent_description, type=textarea])
			]),
			p([], input([name=submit, type=submit, value='Submit'], []))
		])
	]).

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

session_agents_page_content -->
	{
		findall([AgentName, AgentDescription], agent(AgentName, AgentDescription), Agents)
	},
	html([
		h1("Session Agents"),
		p("Select agents for your session"),
		div(\select_agents_form),
		p("Don't know what an agent does? here's the description:"),
		div(\agents_display(Agents))
	]).
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
session_agent("ExampleName").
get_session_agents_page_content(Request) -->
	{
		findall(AgentName, http_session_data(session_agent(AgentName)), Agents)
	},
	html([
		h1("Selected Agents"),
		p("Here are the agents you selected"),
		div(\session_agents_display(Agents))
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
		
:- multifile user:body//2.

user:body(proof_style, Body) -->
        {
                http_link_to_id(home_page, [], HomeHREF),
                http_link_to_id(new_agent, [], NewAgentHREF),
		http_link_to_id(get_agents, [], GetAgentsHREF),
		http_link_to_id(session_agents, [], SessionAgentsHREF)
        },
        html(body([
		div(id(top),[
                	div([a([href(HomeHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'Home'),
				a([href(NewAgentHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'New Agent'),
				a([href(GetAgentsHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'Agents'),
				a([href(SessionAgentsHREF), style="border: 1px solid black; margin: 2px; padding: 2px;"], 'Session agents')
			])
		]),
		div(id(content), Body)
        ])).


/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          new_abstract_path.pl
Created:        4th Oct 2018
Desc:           Part of my proof of concept Prolog service
-------------------------------------------*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(border).
:- use_module(message_of_the_day).

:- http_handler(root(.), home_page, []).
:- http_handler(root(new_agent), new_agent, []).

server(Port):-
        http_server(http_dispatch, [port(Port)]).

home_page(Request):-
	reply_html_page(
		proof_style,
		[title('Prolog Service - Home')],
		[\home_page_content(_Request)]
	).

new_agent(Request):-
	reply_html_page(
		proof_style,
		[title('Prolog Service - New Agent')],
		[\new_agent_page_content(Request)]
	).

home_page_content(_Request) -->
	html([
		h1("Home"),
		p("Welcome to my proof of concept for a Prolog web service"),
		\my_fancy_border(p(["Message of the day: ", \motd]))
	]).

new_agent_page_content(_Request) -->
	html([
		h1("New Agent"),
		p("Create new agents here"),
		\my_fancy_border(\new_agent_form(Request))
	]).

new_agent_form(_Request) -->
	html([
		form([
			div([p('Agent Name:'),
			input([type(text), name(agent_name)], '')]),
			div([p('Agent Description:'),
			input([type(text), name(agent_description)], '')]), br(''),
			input([type(submit), value('Submit')])
		])
	]).

:- multifile user:body//2.

user:body(proof_style, Body) -->
        {
                http_link_to_id(home_page, [], HomeHREF),
                http_link_to_id(new_agent, [], NewAgentHREF)
        },
        html(body([
		div(id(top),[
                	div([a([href(HomeHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'Home'), a([href(NewAgentHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'New Agent')])
		]),
		div(id(content), Body)
        ])).


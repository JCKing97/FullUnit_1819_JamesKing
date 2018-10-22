/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          html2.pl
Created:        7th Oct 2018
Desc:		Further exploration of html in Prolog web services           
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
		[title('Prolog Service - Home')],
		[\home_page_content(_Request)]
	).

new_agent(Request):-
	reply_html_page(
		[title('Prolog Service - New Agent')],
		[\new_agent_page_content(Request)]
	).

home_page_content(_Request) -->
	html([
		h1("Home"),
		div(\nav_bar),
		p("Welcome to my proof of concept for a Prolog web service"),
		\my_fancy_border(p(["Message of the day: ", \motd])),
		div(\html_receive(bottom_nav))
	]).

new_agent_page_content(_Request) -->
	html([
		h1("New Agent"),
		div(\nav_bar),
		p("Create new agents here"),
		\my_fancy_border(\new_agent_form(Request)),
		div(\html_receive(bottom_nav))
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

nav_bar -->
	{
		findall(Name, nav(Name, _), ButtonNames),
		maplist(as_top_nav, ButtonNames, TopButtons),
		maplist(as_bottom_nav, ButtonNames, BottomButtons)
	},
	html([\html_post(bottom_nav, BottomButtons) | TopButtons]).

nav('Home', HREF):-
	http_link_to_id(home_page, [], HREF).
nav('New Agent', HREF):-
	http_link_to_id(new_agent, [], HREF).

as_top_nav(Name, a([href(HREF), class=topnav], Name)):-
	name(Name, HREF).

as_bottom_nav(Name, a([href(HREF), class=bottomnav], Name)):-
	name(Name, HREF).

/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          handlers.pl
Created:        4th Oct 2018
Desc:           The main file of my proof of concept application for a Prolog web service
-------------------------------------------*/

% Modules form the standard libraries required for the http web server and other parts
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_files)).
:- use_module(library(lists)).

% Token modules for the sake of practicing modules
:- use_module(border).
:- use_module(message_of_the_day).

% Modules for content creation and agent management
:- use_module(home).
:- use_module(new_agents).
:- use_module(agents).
:- use_module(session_agents).

% Define a route location for handling agents
:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(agents, '/agents', []).
http:location(agent_selection, '/agent_selection', []).

% Link routes to handlers using the http locations (root is predefine as the index)
:- http_handler(root(.), home_page, []).
:- http_handler(agents(new_agent), new_agent, []).
:- http_handler(agents(get_agents), get_agents, []).
:- http_handler(agent_selection(session_agents), session_agents, []).
:- http_handler(agent_selection(get_session_agents), get_session_agents, []).
:- http_handler(root(forbidden_operation), forbidden_operation, []).

% Handle by serving a static file from the directory assets
http:location(files, '/f', []).
:- http_handler(files(.), http_reply_from_files(assets, []), [prefix]).

% The predicate to start the server on the Port number input
server(Port):-
        http_server(http_dispatch, [port(Port)]).

% Handler for the home page route
home_page(Request):-
	member(method(get), Request), !,
	reply_html_page(
		proof_style,
		[title('Prolog Service - Home')],
		[\home_page_content]
	).
	
% Handlers for get and post requests to the new_agent route
% Display a form to create a new agent
new_agent(Request):-
	member(method(get), Request), !,
	reply_html_page(
		proof_style,
		[title('Prolog Service - New Agent')],
		[\new_agent_page_content]
	).
% Deal with the POST from the form in the handler above, asserting a new agent in the system
new_agent(Request):-
	member(method(post), Request), !,
	http_read_data(Request, [agent_name=AgentName,agent_description=AgentDescription|_], []),
	assert(agents:agent(AgentName, AgentDescription)),
	http_link_to_id(get_agents, [], GetAgentsHREF),
	http_redirect(see_other, GetAgentsHREF, Request).


% Handler for the get_agents route, getting the agents currently in the system and displaying them
% Must deal with post and get requests due to the redirect upon submission of the new_agent form
get_agents(Request):-
	(member(method(post), Request) ; member(method(get), Request)),
	reply_html_page(
		proof_style,
		[title('Prolog Service - Agents')],
		[\get_agent_page_content]
	).

% Handlers for get and post requests to the session_agents route
% Display agents currently in the system and a form to select an agent for the http session
session_agents(Request):-
	member(method(get), Request), !,
	reply_html_page(
		proof_style,
		[title('Prolog Service - Select Agents')],
		[\session_agents_page_content]
	).
% Deal with the POST request by asserting on this session only a selected agent (Assert "No such agent" if the form input is not valid)
session_agents(Request):-
	member(method(post), Request), !,
	http_read_data(Request, [agent_selector=Agent|_], []),
	( agents:agent(Agent, _) -> http_session_assert(session_agents:session_agent(Agent)) ; http_session_assert(session_agents:session_agent("No such agent")) ),
	http_link_to_id(get_session_agents, [], GetSessionAgentsHREF),
	http_redirect(see_other, GetSessionAgentsHREF, Request).
	
% Gets the agents currently selected for this session
get_session_agents(Request):-
	( member(method(get), Request) ; member(method(post), Request) ), !,
	findall(AgentName, http_session_data(session_agents:session_agent(AgentName)), Agents),
	reply_html_page(
		proof_style,
		[title('Prolog Service - Session Agents')],
		[\get_session_agents_page_content(Agents)]
	).

% A user has committed a forbidden operation so throw an error at them
forbidden_operation(Request):-
	member(path(URL), Request),
	throw(http_reply(forbidden(URL))).
		
% In every response hook the user:body non-term into the top of the body
:- multifile user:body//2.

user:body(proof_style, Body) -->
        {
                http_link_to_id(home_page, [], HomeHREF),
                http_link_to_id(new_agent, [], NewAgentHREF),
		http_link_to_id(get_agents, [], GetAgentsHREF),
		http_link_to_id(session_agents, [], SessionAgentsHREF),
		http_link_to_id(forbidden_operation, [], ForbiddenOperationHREF)
        },
        html(body([
		div(id(top),[
                	div([a([href(HomeHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'Home'),
				a([href(NewAgentHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'New Agent'),
				a([href(GetAgentsHREF), style="border: 1px solid black; margin: 2px; padding: 2px"], 'Agents'),
				a([href(SessionAgentsHREF), style="border: 1px solid black; margin: 2px; padding: 2px;"], 'Session agents'),
				a([href(ForbiddenOperationHREF), style="border: 1px solid black; margin: 2px; padding: 2px;"], 'Forbidden operation')
			])
		]),
		div(id(content), Body)
        ])).

% Error handling for 404 errors
:- multifile http:status_page/3.

http:status_page(not_found(URL), _Context, HTML):-
	phrase(page([
		title('404 - Page Not Found')],
		{|html(URL)||
		<h1>Sorry, page not foud</h1>
		<p>It appears the page you requested has not been found</p>|}),
		HTML).

/*------------------------------------------
Author:         Anne Ogborn adapted by James King
Title:          new_abstract_path.pl
Created:        4th Oct 2018
Desc:           Playing around with SWI-Prolog web service paths
-------------------------------------------*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).

% We can add file search paths to add projects into our main project as submodules using library
% user:file_search_path(library, './project').
% Or our own paths to serve certain files, here we set a file search path for agent resources
% and we define a descriptions alias for our agents
:- user:file_search_path(agents, './agents').
:- user:file_search_path(descriptions, agents(descriptions)).
% If this is buggy use the prolog flag verbose_file_search to make prolog print out the absolute directory

% We can add more abstract base locations such as one for the get  mapped to /get
% Further abstract base locations can be based on the other abstract base locations 
:- multifile 	http:location/3.
:- dynamic	http:location/3.
% use the set_setting predicate to change a prolog setting
% In this case the setting http:prefix is set to /amaas
% Changing this setting moves the whole server to a new prefix instead of root alias being / it is now /amaas
:- set_setting(http:prefix, '/amaas').

http:location(get, root(get), []).
http:location(action, get(action), []).
http:location(gossip, get(gossip), []).

:- http_handler(action(cooperator), say_whatever("Cooperate!"), []). % reachable at /get_action/cooperator offset by the abstract base root /get_action using the get_action alias
:- http_handler(action(defector), say_whatever("Defect!"), []). 
:- http_handler(gossip(cooperator/defector), say_whatever("Defector isn't very nice"), []).
:- http_handler(gossip(defector/cooperator), say_whatever("Cooperator never helps out"), []).

:- http_handler(root(.), say_whatever('Hello  World!'), []).

server(Port):-
        http_server(http_dispatch, [port(Port)]).

say_whatever(Whatever, _Request):-
        format('Content-type: text/plain~n~n'),
        format('~w~n', [Whatever]).

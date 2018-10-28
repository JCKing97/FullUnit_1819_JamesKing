/*------------------------------------------
Author:         Anne Ogborn adapted by James King
Title:          abstract_path.pl
Created:        4th Oct 2018
Desc:           Introduction to abstract paths in SWI-Prolog web services
-------------------------------------------*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).

% Bind an http path to a predicate.
% root/1 is already defined and works as an alias mechanism
% Allows moving parts of the server URI endpoints easily
:- http_handler(root(.), say_whatever('Hello  World!'), []).
:- http_handler(root(taco), say_whatever('Tacos are yummie!'), []). % reachable at /taco offset by the abstract base root which is /

server(Port):-
	http_server(http_dispatch, [port(Port)]).

say_whatever(Whatever, _Request):-
	format('Content-type: text/plain~n~n'),
	format('~w~n', [Whatever]).


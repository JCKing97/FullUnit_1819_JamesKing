/*-------------------------------------------
Author:		Anne Ogborn adapted by James King
Title:		helloweb.pl
Created:	4th Oct 2018
Desc:		A hello world web program in SWI-Prolog
-------------------------------------------*/

% Two modules needed for a server to send and receive messages
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% Declare a handler for a specific URI endpoint '/'
% The function to handle it 'say_hi'
% The third argument contains options
% Similar to Flask decorators
:- http_handler(/, say_hi, []).

% Starts the server on the port number passed in Port
% Creates a number of threads and returns to the top level
server(Port):-
	http_server(http_dispatch, [port(Port)]).

% The function for handling a request on URI endpoint '/'
% _Request provides the requests details
% The first format is part of a CGI document (name: value pairs) followed by two new lines
% Content-type is required for this CGI document
% format/2 is how we print, there are other options
say_hi(_Request):-
	format('Content-type: text/plain~n~n'),
	format('Hello World!~n').

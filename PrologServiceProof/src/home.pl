/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          home.pl
Created:        4th Oct 2018
Desc:           The file containing content creation for the home page
-------------------------------------------*/

:- module(home, [home_page_content//0]).

:- use_module(border).
:- use_module(message_of_the_day).
    
home_page_content -->
	html([
		h1("Home"),
		p("Welcome to my proof of concept for a Prolog web service"),
		\my_fancy_border(p(["Message of the day: ", \motd]))
	]).

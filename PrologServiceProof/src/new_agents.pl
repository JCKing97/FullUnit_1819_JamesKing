/*-------------------------------------------
Author:         James King adapted from Anne Ogborn 
Title:          new_agents.pl
Created:        4th Oct 2018
Desc:           The file containing content creation for the new agents page
-------------------------------------------*/

:- module(new_agents, [new_agent_form//0, new_agent_page_content//0]).

:- use_module(border).

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

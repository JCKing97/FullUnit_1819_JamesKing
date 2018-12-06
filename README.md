# FullUnit_1819_JamesKing

My project title: Cooperative Strategies in Multi-Agent Systems

My interim report is found in the same directory as this file and has more details.

In this README I shall be describing the programs I have written and planned.

# Programs

This is a bried description of my programs and how to run them. Background theory and more details are provided in my interim report in the file InterimReport.pdf

## Prolog Service Proof of Concept

Kept in the PrologServiceProof directory.
To run this make sure you have SWI-Prolog installed, it has been run and tested on version 7.6.4.
To run navigate to the directory PrologServiceProof/src in a terminal and run the command prolog main.pl
This will compile the Prolog files and open a Prolog environment. In this terminal run the predicate: server(<port_num>) with port_num as the port number you want to run it on such as 5000 or 8080.
You will then be able to access the web service on http://localhost:<port_num>/
The web service has a simple home page, with links in a banner across the top.
The New Agent page allows you to input a new agent into the service using the form. The Agents page allows you to view the agents currently in the system. The Session Agents page allows you to select an agent for your current session from the agents in the system (appending it to the current session agents).
The reason for this proof of concept was to learn to write and practice writing web services in Prolog. It was done by following a tutorial online for Prolog web applications and as such is mostly code adapted from that tutorial (the link to which is in the bibliography of my interim report).
The implementation is split across a number of modules with most of the work in main.pl and the other modules including agents.pl, new_agents.pl and session_agents.pl implementing the html and front-end code. This also allowed me to practice my Prolog skills, such as using modules.
More details can be found in my summary of completed work chapter in my interim report.

## Agents Service

Kept in the AgentsService directory.
Again make sure you have SWI-Prolog installed, it has been run and tested on version 7.6.4.
To run this navigate to the directory AgentsService/src and run the command prolog main.pl
Like the proof of concept this will compile the Prolog files and open a Prolog environment. In this terminal run the predicate: server(<port_num>) with port_num as the port number you want to run it on such as 5000 or 8080.
You will then be able to access the web service on http://localhost:<port_num>/
This service is not built to deliver any kind of front-end but work as an API for applications/developers to use. If you go to the URL above you will receive a JSON file containing all the strategies available in the service.
The API is documented in the file main.raml, converted to a user-understandable format in the file main.html under the AgentsService/api_docs directory. Access this file using a javascript enabled web browser. You can the use the API as you wish.
Th functionality is mostly explained in this html, but as an overciew the API allows you to create communities, generations and agents in the service and the input percepts to these agents, query agents beliefs and ask the agent for a commitment to an action.
This API has been tested using the python requests library and unittest framework. The tests for the API can be found under the directory NatureEngineWebApp/tests/agents_service_testing.

## The Nature Engine

Kept in the NatureEngineWebApp directory.
For this make sure you have Python 3 and pip3 installed, it has been run and tested on Python 3.6.
The other requirements to run the project are kept in the requirements.txt file, there is a virtual environment already setup under the venv directory.

To run the web application take the following steps.
Make sure the agents service is running as described above.
If there is no file app.db or the system returns an exception relating to the SQLAlchemy ORM do this:
In a terminal activate the virtual environment and run the commands: flask db init, flask db migrate, flask db upgrade
This should sort out any database issues
To be able to run a tournament of direct reciprocity a redis queue worker needs to be active, to activate one do this:
In a terminal activate the virtual environment and run the command: rq worker nature_engine_tasks
To run the flask web application keep the virtual environment active and set the FLASK_APP environment variable:
For windows: set FLASK_APP=app/__init__
For linux and mac: export FLASK_APP=app/__init__
Finally execute the command: flask run
You will now be able to access the web application on http://localhost:5000/

You will be greeted by a home screen which has a number of pictures and areas to contain information on what the website is about and very high-level theory on the evolution of cooperation.
On the banner at the top of the web page you can use the links to navigate the website. If you click on the match link you can select two strategies for agents and the length of a match, run the match and view an analysis of what happened.
If you click on the tournament link you can select a number of strategies (must be greater than two and less than fifty) and run a tournament. Currently there is no tournament analysis page as I moved on from the proof of concept applications to start getting other project aspects done first, I will come back to this.
The reputation page displays a number of strategies from the agents service which you can select. However selecting them and running them will not results in any simulation of a tournament as they are not currently correctly linked up. This is a task that I shall be completing soon.
In all these pages you can select agents description of the agents are given in an information box below.

The configuration of the web app is implemented in the file NatureEngineWebApp/config.py file. NatureEngineWebApp/app/models.py contains the logic for the database, in the same directory __init__.py handles the initialization of the flask application. Logic for nice error pages when a 404 or 500 error occurs is implemented in the NatureEngineWebApp/errors directory. Logic for the Reputation/indirect reciprocity part of my application including the environment is under teh NatureEngineWebApp/app/indir_rec directory, more about this below. The logic to run the home page and direct reciprocity sections of the web application is in the NatureEngineWebApp/app/main directory, this includes the routes, conversion of data into the database the form for the match and running of the matches and tournaments. The NatureEngineWebApp/app/static/images directory contains the images for the home page. Finally the NatureEngineWebApp/app/templates directory contains all the jinja2 templates for the front-end of the web pages, including the React code for selecting agents for a tournament.


## The Environment

This is part of the Nature Engine web application, but requires special mention as it is the environment of my multi-agent system for running indirect reciprocity.
The logic for this is implemented in the NatureEngineWebApp/app/indir_rec directory.
The logic implemented included th creation of the relevant players, generations and a community in the agents service and then simulating a number of generations using a reproduction algorithm implemented in the Community class. Each generation is simulated by creating a Generation object and then calling simulate where the program executes in a cycle the steps: perceive, decide and execute. Perceive is sending all the relevant percepts to agents, decide is getting commitments from agents as to what they want to do at the specified timepoint and execute is generating percepts from these actions and if necessary updating player fitness. The class Player acts as the body of the agent and also an interface between the Generation and the agents service for agent decisions.

Unfortunately this is not linked up to the front-end as it is very much still in development.
For each logic file containing the business logic there are TDD unittests for the classes. You can run these tests to see how the logic works.
An especially interesting test to run is in the file community_tests.pl in the CommunityTest class under the function test_simulate. Running this may take a while as this requires a lot of interaction back and forth between the environment and agents and also a lot of computation by the environment, but this is to be expected.
The reasoning the test is interesting is the output. Ignore the pending deprecatin warning, as this is nothing to do with my code but is from the Axelrod-Python library, but if you scroll up you are able to see the community simulation in action with a print out of interesting percepts and decisions by agents.
To run the tests activate the virtual environment navigate to the NatureEngineWebApp/ directory and run the command python3 -m unittest app.indir_rec.community_tests

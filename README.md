# FullUnit\_1819\_JamesKing

Project title: Cooperative Strategies in Multi-Agent Systems

In this README I shall be describing the programs I have written and planned.

# Programs

This is a brief description of my programs and how to run them. Background theory and more details are provided in my final report.

## Prolog Service Proof of Concept

Kept in the PrologServiceProof directory.
To run this make sure you have SWI-Prolog installed, it has been run and tested on version 7.6.4.
To run navigate to the directory PrologServiceProof/src in a terminal and run the command "prolog main.pl".
This will compile the Prolog files and open a Prolog environment. In this terminal run the predicate: "server(PORT)" with PORT as the port number you want to run it on such as 5000 or 8080.

You will then be able to access the web service on http://localhost:PORT/
The web service has a simple home page, with links in a banner across the top.
The New Agent page allows you to input a new agent into the service using the form. The Agents page allows you to view the agents currently in the system. The Session Agents page allows you to select an agent for your current session from the agents in the system (appending it to the current session agents).
The reason for this proof of concept was to learn to write and practice writing web services in Prolog. It was done by following a tutorial online for Prolog web applications and as such is mostly code adapted from that tutorial (the link to which is in the bibliography of my interim report).
The implementation is split across a number of modules with most of the work in main.pl and the other modules including agents.pl, new\_agents.pl and session\_agents.pl implementing the HTML and front-end code. This also allowed me to practice my Prolog skills.
More details can be found in my summary of completed work chapter in my interim report.

## Agents Service

Kept in the AgentsService directory.
Again make sure you have SWI-Prolog installed, it has been run and tested on version 7.6.4 and 8.0.2.
To run this navigate to the directory AgentsService/src and run either the command "prolog main.pl" or "prolog run.pl"
If you use "prolog run.pl" a server will be spun up on the port 8080. 8080 is the recommended port for working with The Nature Engine web app, unless you set the environment variable AGENTS_URL to the url with the port you wish i.e. "http://localhost:PORT/".
To run the service on the port you wish use "prolog main.pl" and then run the predicate: server(PORT) with PORT as the port number you want to run the service on.
You will then be able to access the web service on http://localhost:PORT/

This service is not built to deliver any kind of web page front-end but work as an API for applications/developers to use. If you go to the URL above you will receive a JSON file containing all the strategies available in the service.
The API is documented in the file main.raml, converted to a user-understandable format in the file main.html under the AgentsService/api\_docs directory. Access this file using a javascript enabled web browser. You can then use the API as you wish, maybe testing it with Postman or using it for your own app.
The functionality is mostly explained in this HTML, but as an overview the API allows you to create communities, generations and agents - that can use a variety of available strategies - in the service and then input percepts to these agents, query agents beliefs and ask the agents for commitments to actions.
This API has been tested using the python requests library and unittest framework. The tests for the API can be found under the directory NatureEngineWebApp/tests/api\_testing.

## The Nature Engine

Kept in the NatureEngineWebApp directory.
We will be installing the requirements in a terminal, these commands are correct for Linux however they may differ for Windows and Mac. To the best of my knowledge the commands are similar.
For this make sure you have Python 3.6.7 or later, pip3 and virtualenv installed.
The other requirements to run the project are kept in the requirements.txt file.
To install the requirements navigate to the the NatureEngineWebApp directory and run the command "python3 -m virtualenv venv".
Then activate the virtual environment with the command "source venv/bin/activate".
From here run the command "pip3 install -r requirements.txt" and then all the requirements will be installed.

The web application also requires a database and a Redis-server to run, make sure sqlite3 and Redis is installed.
To create the database keep the virtual environment activated and be in the NatureEngineWebApp directory.
From here run the commands "flask db migrate" and then "flask db upgrade", this creates all the relevant database tables.


To run the web application take the following steps.
Make sure the agents service is running as described above in a terminal tab or window.
To be able to run a tournament of direct reciprocity and a game of mixed reciprocity a Redis queue worker needs to be active, to activate one do this:
In another terminal tab or window activate the virtual environment ("source venv/bin/activate" from the NatureEngineWebApp directory) and run the command "rq worker nature\_engine\_tasks"

To run the flask web application open another terminal tab or window activate the virtual environment and set the FLASK\_APP environment variable:
For Windows: set FLASK\_APP=app/\_\_init\_\_
For Linux and Mac: export FLASK\_APP=app/\_\_init\_\_
Finally, execute the command "flask run".
You will now be able to access the web application on http://localhost:5000/

You will be greeted by the home page which has a carousel of pictures and text describing the high-level theory surrounding the application's purpose.
On the banner at the top of the web page, you can use the links to navigate the website. 
If you click on the match link you can select two strategies for agents and the length of a match, run the match and view an analysis of what happened.
If you click on the tournament link you can select a number of strategies (must be greater than two and less than fifty) and run a tournament. After the tournament has been run in a Redis worker an analysis page will be displayed.
The reputation page displays a number of strategies and parameters you can select to set up a reputation game. Run the game by pressing submit. A Redis worker will then run in the background and when finished you will be redirected to an analysis page which you can view statistics and an animation of the game.
In all these pages you can select agents description of the agents are given in an information box below.

The configuration of the web app is implemented in the file NatureEngineWebApp/config.py file. NatureEngineWebApp/app/models.py contains the logic for the database, in the same directory __init__.py handles the initialization of the flask application. Logic for nice error pages when a 404 or 500 error occurs is implemented in the NatureEngineWebApp/errors directory. Logic for the Reputation game is under the NatureEngineWebApp/app/indir\_rec directory, more about this below. The logic to run the home page and direct reciprocity sections of the web application is in the NatureEngineWebApp/app/main directory, this includes the routes, conversion of data into the database the form for the match and running of the matches and tournaments. The NatureEngineWebApp/app/static/images directory contains the images for the home page. Finally, the NatureEngineWebApp/app/templates directory contains all the jinja2 templates for the web pages, including the React.js code for selecting agents for a tournament and the animation code for reputation games.


## Reputation Game

Housed in the NatureEngineWebApp/app/indir\_rec directory, multi-agent interactions are modelled in a game-like multi-agent system. These games require the Agent Service to be running on port 8080 in order to use the strategies implemented in the service. 
A game can be run separately to the web application using the python interpreter. To do this activate the virtual environment and run the command "python3".
Once the interpreter prompts you, you will need to extend the system path to point it to the project using the command "sys.path.extend([FULL_PATH])" where FULL_PATH is the full path to the NatureEngineWebApp directory as a string.
You can then import the correct classes using "from app.indir_rec.facade_logic import Results, ReputationGame".
You can then create a new ReputationGame object "game = ReputationGame(population, num_of_onlookers, num_of_generations, length_of_generations, mutation_chance)". This setup is similar to the form on the web app except the population is a list containing dictionaries of the agent strategies, for example:
{
	"donor_strategy": "Veritability Discerner",
	"non_donor_strategy": "Lazy",
	"trust_model": "Balanced Reactor",
	"options": [5],
	"count": 6 
}

You will need to use the strategies from the agent service. To find these open a web browser and navigate to the url "http://localhost:8080/" and a json will describe to you the strategies available. The "count" key points to the number of this specific strategy you wish to include in the initial population.
You can then run the game, and view an analysis using the returned Results object with "results = game.run()".
This runs a game which you can find described in my final report in the Theoretical Framework section.
The tests are contained alongside the business logic of in separate packages, for example, the tests for the "facade_logic.py" package are tested in "facade_tests.py".

Please note: running games runs a genetic algorithm combined with a multi-agent system, this takes a lot of computational resources and time. The longer the num_of_generations and length_of_generations you set the longer it will take to run this. I would suggest beginning with a num_of_generations around 5 and a length_of_generations around 20-40. 
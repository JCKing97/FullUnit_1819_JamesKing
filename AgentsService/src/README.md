# Agent Mind as a Service

The agent mind as a service system hosts and manages agents minds for games of indirect reciprocity.
The service provides an API through which to interact, documented in the AgentsService/api_docs folder.
The agents minds can use the strategies implemented in the system, accessible at the /strategy uri.
Tested in SWI-Prolog version 7.6.4

# Running the service

To run the service run prolog and compile the main.pl file under the src directory.
Then execute the predicate: server(Port) with Port as the port you want to run it on for example 8080 or 5000 (preferably 8080 when running with the nature engine).
Then use the uri endpoints and http methods specified in the api docs.

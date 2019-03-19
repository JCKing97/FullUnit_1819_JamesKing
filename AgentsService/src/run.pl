/** <module> This file runs the service on compilation
 * @author James King
 */

?-['./main'].

:- initialization main.

% Allows running of the server from the command line
main:-
    server(8080).


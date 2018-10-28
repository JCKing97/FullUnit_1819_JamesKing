/*-------------------------------
Author:		James King adapted from Anne Ogborn
-------------------------------*/
:- module(message_of_the_day, [motd//0]).

message(Message):-
        Message = "Indirect reciprocity is cool".

motd -->
        {message(Message)},
        html([ins(Message)]).

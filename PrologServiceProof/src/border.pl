/*---------------------------------
Author: 	James King adapted from Anne Ogborn
--------------------------------*/
:- module(border, [my_fancy_border//1]).

my_fancy_border(InnerHTML) -->
        html([
                div(style="border: 1px solid black; padding: 5px", InnerHTML)
        ]).


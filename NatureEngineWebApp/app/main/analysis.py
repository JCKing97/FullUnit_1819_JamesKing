"""A module for analysis on matches, tournaments etc."""

__author__ = "James King"


def get_match_points(interaction_history):
    """Gets the points for the 2 players in a single match
    :rtype: a dictionary
    :return: if there is no interaction history an empty dictionary, else a dictionary containing
     the keys of the players and their respective points earned"""
    player_points = {}
    round_history = {}
    for action in interaction_history:
        player_points[action.player_id] = 0
        try:
            round_history[action.round_num][action.player_id] = action.cooperate
        except KeyError:
            round_history[action.round_num] = {action.player_id: action.cooperate}
    players = [player for player in player_points.keys()]
    for roundkey, roundvalue in round_history.items():
        if roundvalue[players[0]] is True and roundvalue[players[1]] is True:
            player_points[players[0]] += 3
            player_points[players[1]] += 3
        elif roundvalue[players[0]] is True:
            player_points[players[1]] += 5
        elif roundvalue[players[1]] is True:
            player_points[players[0]] += 5
        else:
            player_points[players[0]] += 1
            player_points[players[1]] += 1
    return player_points


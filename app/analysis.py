"""A module for analysis on games, tournaments etc."""


def get_game_points(interaction_history):
    """Gets the points for the 2 players in a single game
    :rtype: a dictionary
    :return: if there is no interaction history an empty dictionary, else a dictionary containing the keys of the players and their respective points earned"""
    player_points = {}
    round_history = {}
    for action in interaction_history:
        player_points[action.agent_id] = 0
        try:
            round_history[action.round_num][action.agent_id] = action.cooperates
        except KeyError:
            round_history[action.round_num] = {action.agent_id: action.cooperates}
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


def get_tournament_points(games):
    """Get the points for agents throughout a tournament
    :rtype: a dictionary
    :return: if there are no games an empty dictionary, else keys are players and their values are their respective points earned throughout the tournament"""
    player_points = {}
    for game in games:
        interaction_history = game.get_interaction_history()
        player_game_points = get_game_points(interaction_history)
        for player in player_game_points:
            try:
                player_points[player] += player_game_points[player]
            except KeyError:
                player_points[player] = player_game_points[player]
    return player_points



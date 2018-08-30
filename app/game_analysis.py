import copy


def get_points(interaction_history):
    player_points = {}
    interaction_history_copy = copy.deepcopy(interaction_history)
    for key in interaction_history_copy[0]:
        player_points[key] = 0
    players = [player for player in player_points.keys()]
    for round in interaction_history_copy:
        if round[players[0]] is True and round[players[1]] is True:
            player_points[players[0]] += 3
            player_points[players[1]] += 3
        elif round[players[0]] is True:
            player_points[players[1]] += 5
        elif round[players[1]] is True:
            player_points[players[0]] += 5
        else:
            player_points[players[0]] += 1
            player_points[players[1]] += 1
    return player_points

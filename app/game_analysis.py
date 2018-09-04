import copy


def get_points(interaction_history):
    player_points = {}
    round_history = {}
    for action in interaction_history:
        player_points[action.agent_id] = 0
        if round_history[action.round_id]:
            round_history[action.round_id][action.agent_id] = action.cooperates
        else:
            round_history[action.round_id] = {action.agent_id: action.cooperates}
    players = [player for player in player_points.keys()]
    for round in round_history:
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

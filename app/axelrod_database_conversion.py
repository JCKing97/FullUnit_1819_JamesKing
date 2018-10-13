from app import db
from app.models import Match, Round, Player, Action
import axelrod

def match_result_to_database(results, players):
    m = Match()
    db.session.add(m)
    db.session.flush()
    players = [Player(id=i, match_id=m.id, strategy=players[i].name) for i in range(0,2)]
    db.session.add_all(players)
    db.session.flush()
    i = 1
    for round in results:
        r = Round(num=i, match_id=m.id)
        db.session.add(r)
        db.session.flush()
        for j in range(0, 2):
            a = Action(round_num=r.num, match_id=m.id, player_id=players[j].id, cooperate=(round[j]==axelrod.Action.C))
            db.session.add(a)
        i += 1
    db.session.commit()
    return m.id


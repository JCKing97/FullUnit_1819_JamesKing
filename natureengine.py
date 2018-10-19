from app import create_app, db
from app.models import Match, Round, Player, Action, Tournament

app = create_app()


@app.shell_context_processor
def make_shell_context():
    return {'db': db, 'Match': Match, 'Round': Round, 'Player': Player, 'Action': Action, 'Tournament': Tournament}

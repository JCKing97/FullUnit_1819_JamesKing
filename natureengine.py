from app import app, db
from app.models import Strategy, Game, Action, Agent


@app.shell_context_processor
def make_shell_context():
    return {'db': db, 'Strategy': Strategy, 'Game': Game, 'Agent': Agent, 'Action': Action}

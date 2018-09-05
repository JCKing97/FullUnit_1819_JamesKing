import unittest
from app import app, db
from app.models import Strategy, Game, Agent, Action


class GameModelCase(unittest.TestCase):
    def setUp(self):
        app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite://'
        db.create_all()

    def tearDown(self):
        db.drop_all()



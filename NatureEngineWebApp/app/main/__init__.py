from flask import Blueprint

__author__ = "James King adapted from Miguel Grinberg's Flask Mega Tutorial"

bp = Blueprint('main', __name__)

from app.main import routes
from flask import Blueprint

__author__ = "James King"

bp = Blueprint('indir_rec', __name__)

from app.indir_rec import routes
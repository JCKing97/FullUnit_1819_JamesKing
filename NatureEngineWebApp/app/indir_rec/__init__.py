"""This package contains the indirect reciprocity environment.
 The environment includes the functionality for simulating and getting statistics on my model of indirect reciprocity"""

from flask import Blueprint

__author__ = "James King"

bp = Blueprint('indir_rec', __name__)

from app.indir_rec import routes
"""This package contains functionality to handle the main routes of the website
 (anything but errors and indirect reciprocity). This is the running and analysis of the Iterated Prisoner's Dilemma
 matches and tournaments, and also the functionality for the home and about page routes."""

from flask import Blueprint

__author__ = "James King adapted from Miguel Grinberg's Flask Mega Tutorial"

bp = Blueprint('main', __name__)

from app.main import routes
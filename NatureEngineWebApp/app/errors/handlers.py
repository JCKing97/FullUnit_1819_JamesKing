"""handlers.py: A module to handle common web application errors such as 404 and 500"""

__author__ = "Miguel Grinberg"

from flask import render_template
from app.errors import bp
from app import db


@bp.app_errorhandler(404)
def not_found_error(error):
    """The template to render when a 404 errors occurs"""
    return render_template('errors/404.html'), 404


@bp.app_errorhandler(500)
def internal_error(error):
    """The template to render when a 500 errors occurs"""
    db.session.rollback()
    return render_template('errors/500.html'), 500

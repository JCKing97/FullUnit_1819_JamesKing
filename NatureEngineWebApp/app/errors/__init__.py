"""This package contains logic to hook in error handlers."""

from flask import Blueprint

__author__ = "Miguel Grinberg"

bp = Blueprint('errors', __name__)

from app.errors import handlers
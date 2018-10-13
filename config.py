"""config.py: A python module to build a configuration for when the web app is in production and development"""

__author__ = "James King adapted from Miguel Grinberg Flask Mega Tutorial"

import os
basedir = os.path.abspath(os.path.dirname(__file__))


class Config(object):
    """"Contains config information for the running of the server"""
    LOG_TO_STDOUT = os.environ.get('LOG_TO_STDOUT')
    SECRET_KEY = os.environ.get('SECRET_KEY') or 'you-will-never-guess'
    SQLALCHEMY_DATABASE_URI = os.environ.get('DATABASE_URL') or \
        'sqlite:///' + os.path.join(basedir, 'app.db')
    SQLALCHEMY_TRACK_MODIFICATIONS = False

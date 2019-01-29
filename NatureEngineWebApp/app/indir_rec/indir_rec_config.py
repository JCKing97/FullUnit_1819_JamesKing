import os


class Config:
    AGENTS_URL = os.environ.get('AGENTS_URL') or 'http://127.0.0.1:8080/'

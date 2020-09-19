import os
from .config import BaseConfiguration


class ProductionConfig(BaseConfiguration):
    DEBUG = False
    DB_SERVER_URI = 'postgres://thuger_db:donotusethispassword@localhost:5432/rpserver'
    SECRET_KEY = os.environ.get('SECRET_KEY')


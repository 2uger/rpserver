class Configuration():
    DEBUG = True
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    SQLALCHEMY_DATABASE_URI = 'postgresql+psycopg2://thuger_db:thuger_db@localhost/ridersPlatform'
    #SQLALCHEMY_DATABASE_URI = 'postgresql+psycopg2://postgres:postgres@localhost/ridersPlatform'
    SECRET_KEY = "SECRET_KEY"



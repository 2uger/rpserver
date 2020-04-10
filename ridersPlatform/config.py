class Configuration():
    DEBUG = True
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    SQLALCHEMY_DATABASE_URI = 'postgresql+psycopg2://thuger:thuger@localhost/riders'
    SECRET_KEY = "SECRET_KEY"



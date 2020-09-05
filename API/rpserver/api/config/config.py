class Configuration():
    DEBUG = True
    SECRET_KEY = "SECRET_KEY"
    MAX_CONTENT_LENGTH = 1000

class DevelopmentConfig(Config):
    DB_SERVER = 'localhost'
    DEBUG = True




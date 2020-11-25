class BaseConfiguration():
    DEBUG = True
    SECRET_KEY = "SECRET_KEY"
    MAX_CONTENT_LENGTH = 1000
    BCRYPT_COST_FACTOR = 10
    EMAIL_LOGGING = ''
    DB_SERVER_URI = ''


class DevelopmentConfig(BaseConfiguration):
    DB_SERVER_URI = "postgresql://postgres:postgres@localhost:5432/rpserver" 




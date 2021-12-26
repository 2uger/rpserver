from .base import BaseConfiguration 


class DevelopmentConfig(BaseConfiguration):
    DB_SERVER_URI = "postgresql://postgres:password@db:5432/rpserver"
    DB_CONNECTIONS_MIN = 2
    DB_CONNECTIONS_MAX = 4

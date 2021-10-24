from .base import BaseConfiguration 


class DevelopmentConfig(BaseConfiguration):
    DB_SERVER_URI = "postgresql://postgres:postgres@0.0.0.0:5432/rpserver" 

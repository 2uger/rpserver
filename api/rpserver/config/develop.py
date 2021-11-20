from .base import BaseConfiguration 


class DevelopmentConfig(BaseConfiguration):
    DB_SERVER_URI = "postgresql://postgres:password@192.168.100.19:6432/rpserver" 

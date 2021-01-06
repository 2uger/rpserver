from config.config import DevelopmentConfig
from rpserver.main import backend_app, auth_app

app = auth_app(DevelopmentConfig)
    

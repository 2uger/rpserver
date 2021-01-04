from config.config import DevelopmentConfig
from rpserver.main import backend_app

app = backend_app(DevelopmentConfig)
    

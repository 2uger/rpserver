from werkzeug.middleware.dispatcher import DispatcherMiddleware

from flask import Flask
from rpserver.main import api_app, auth_app
from rpserver.config import DevelopmentConfig, ProductionConfig

app = Flask(__name__)

app.config['ENV'] = 'development'
config = ProductionConfig if app.config['ENV'] == 'production' else DevelopmentConfig
app.config.from_object(config)

app.wsgi_app = DispatcherMiddleware(app.wsgi_app, {"/auth": auth_app(config),
                                          "/api": api_app(config)})

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=5000)

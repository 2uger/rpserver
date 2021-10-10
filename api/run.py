from flask import Flask
from werkzeug.middleware.dispatcher import DispatcherMiddleware

from rpserver.main import auth_app, api_app
from rpserver.config.develop import DevelopmentConfig

#app = Flask(__name__)

#app.wsgi_app = DispatcherMiddleware(app, {"/auth": auth_app(DevelopmentConfig),
#                                          "/api": api_app(DevelopmentConfig)})

if __name__ == "__main__":
    api_app(DevelopmentConfig).run()

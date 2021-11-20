from werkzeug.middleware.dispatcher import DispatcherMiddleware

from flask import Flask
from rpserver.main import api_app, auth_app
from rpserver.config.develop import DevelopmentConfig
from rpserver.exception import exception_handlers 

app = Flask(__name__)

app.config.from_object(DevelopmentConfig)

app.wsgi_app = DispatcherMiddleware(app, {"/auth": auth_app(DevelopmentConfig),
                                          "/api": api_app(DevelopmentConfig)})

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=5000)
    auth_app(DevelopmentConfig).run(host='0.0.0.0', port=5001)
    api_app(DevelopmentConfig).run(host='0.0.0.0', port=5000)

import logging


from flask import Flask, current_app, Blueprint, g, make_response
import psycopg2 as engine


from rpserver.api.config.config import BaseConfiguration

from rpserver.api.middleware.token_auth import jwt_token_authorization
from rpserver.api.middleware.exception import exception_handlers


def db_connection():
    """ Pushing db to app context for each request """

    if "db_connection" not in g:
        with engine.connect(current_app.config.get("DB_SERVER_URI")) as conn:
            g.db_connection = conn
            current_app.logger.info("Creating connection to db")


def create_app(config_class=BaseConfiguration):
    """Initialize main app object"""

    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creating main FLASK object")

    for exception in exception_handlers.keys():
        app.register_error_handler(exception, exception_handlers[exception])

    app.before_request_funcs = {"auth": [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    
    # from rpserver.api.handlers.user import user_bp
    # from rpserver.api.handlers.spot import spot_bp
    # from rpserver.api.handlers.event import event_bp
    from rpserver.api.handlers.auth import auth_bp

    # app.register_blueprint(user_bp, url_prefix='/user')
    # app.register_blueprint(spot_bp, url_prefix='/spot')
    # app.register_blueprint(event_bp, url_prefix='/event')
    app.register_blueprint(auth_bp, url_prefix='/auth')
    

    return app


def shutdown_session(exception=None):
    db_connection = g.pop("db_connection", None)
    if db_connection is not None:
        db_connection.commit()
        db_connection.close()
        current_app.logger.info("Closing db connection")





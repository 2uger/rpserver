import logging

from flask import Flask, current_app, Blueprint, g, make_response
from flask_cors import CORS
import psycopg2 as engine


from rpserver.config.config import BaseConfiguration

from rpserver.api.middleware.token_auth import token_auth
from rpserver.api.exception import exception_handlers 


logging.basicConfig(level=logging.DEBUG)


def auth_app(config_class=BaseConfiguration):
    """ Init app object for auth server """
    app = Flask(__name__)
    CORS(app)
    app.config.from_object(config_class)
    app.logger.info("Creating auth server")

    for key, value in exception_handlers.items():
        app.register_error_handler(key, value)

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    
    from rpserver.auth.handlers import auth_bp
    app.register_blueprint(auth_bp, url_prefix='/auth')

    return app


def api_app(config_class=BaseConfiguration):
    """ Initialize main app object """

    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creating API server")

    for key, value in exception_handlers.items():
        app.register_error_handler(key, value)

    app.before_request_funcs = {None: [token_auth, db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    from rpserver.api.handlers.rider import rider_bp
    from rpserver.api.handlers.spot import spot_bp
    from rpserver.api.handlers.event import event_bp

    app.register_blueprint(rider_bp, url_prefix='/riders')
    app.register_blueprint(spot_bp, url_prefix='/spots')
    app.register_blueprint(event_bp, url_prefix='/events')
    
    return app


def db_connection():
    """ Pushing db to app context for each request """

    if "db_connection" not in g:
            conn = engine.connect(current_app.config['DB_SERVER_URI'])
            g.db_connection = conn
            current_app.logger.info("Creating connection to db")


def shutdown_session(exception=None):
    """ Calls at the end of all requests """
    
    db_connection = g.pop("db_connection", None)
    if db_connection is not None:
        # If error occure, exception will rollback transaction
        # This commit has no effect
        db_connection.commit()
        current_app.logger.info('Commit to database')
        db_connection.close()
        current_app.logger.info("Closing db connection")





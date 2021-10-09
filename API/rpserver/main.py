import logging

from flask import Flask, current_app, Blueprint, g, make_response
from flask_cors import CORS
import psycopg2 as engine
from psycopg2.extras import DictCursor
from werkzeug.exceptions import HTTPException

from rpserver.api.middleware.token_auth import token_auth
from rpserver.api.exception import exception_handlers 


def handle_exception(e):
    return make_response({"error": "Congrats"})

def auth_app(config_class=None):
    """ Init app object for auth server."""
    app = Flask(__name__)
    CORS(app)
    app.config.from_object(config_class)
    app.logger.info("Creating auth server")

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    #app.handle_exception = handle_exception
    
    from rpserver.auth.handlers import auth_bp
    app.register_blueprint(auth_bp, url_prefix='/auth')

    return app


def api_app(config_class=None):
    """ Initialize main app object."""
    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creating API server")

    CORS(app)
    app.before_request_funcs = {None: [token_auth, db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    #app.handle_exception = handle_exception
    for key, value in exception_handlers.items():
        app.register_error_handler(key, value)

    from rpserver.api.rider import rider_bp
    from rpserver.api.spot import spot_bp
    from rpserver.api.event import event_bp

    app.register_blueprint(rider_bp, url_prefix='/riders')
    app.register_blueprint(spot_bp, url_prefix='/spots')
    app.register_blueprint(event_bp, url_prefix='/events')
    app.m = 2
    
    return app


def db_connection():
    """Pushing db to app context for each request."""
    current_app.m += 1
    print(current_app.m)
    if "db_connection" not in g:
            conn = engine.connect(current_app.config['DB_SERVER_URI'], cursor_factory=DictCursor)
            g.db_connection = conn
            current_app.logger.info("Creating connection to db")


def shutdown_session(exception=None):
    """Calls at the end of all requests."""
    current_app.logger.info("Call shutdown session func")

    db_connection = g.pop("db_connection", None)
    if db_connection:
        # If error occure, exception will rollback transaction
        # This commit has no effect
        db_connection.commit()
        current_app.logger.info('Commit to database')
        db_connection.close()
        current_app.logger.info("Closing db connection")

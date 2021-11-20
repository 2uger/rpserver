import logging

from flask import Flask, current_app, Blueprint, g, make_response
from flask_cors import CORS
from psycopg2.extras import DictCursor
from psycopg2.pool import ThreadedConnectionPool
from werkzeug.exceptions import HTTPException

from rpserver.auth.token_auth import token_auth
from rpserver.exception import exception_handlers 


def auth_app(config_class=None):
    """ Init app object for auth server."""
    app = Flask(__name__)
    CORS(app)
    app.config.from_object(config_class)
    app.logger.info("Creating auth server")

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    #app.handle_exception = handle_exception
    
    from rpserver.auth import auth_bp
    app.register_blueprint(auth_bp, url_prefix='/')

    app.db_connection_pool = ThreadedConnectionPool(10, 30, app.config['DB_SERVER_URI'], cursor_factory=DictCursor)
    return app


def api_app(config_class=None):
    """ Initialize main app object."""
    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creating API server")

    CORS(app)
    app.before_request_funcs = {None: [token_auth, db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    for key, value in exception_handlers.items():
        app.register_error_handler(key, value)

    from rpserver.rider import rider_bp
    from rpserver.spot import spot_bp
    from rpserver.event import event_bp

    app.register_blueprint(rider_bp, url_prefix='/riders')
    app.register_blueprint(spot_bp, url_prefix='/spots')
    app.register_blueprint(event_bp, url_prefix='/events')

    app.db_connection_pool = ThreadedConnectionPool(10, 30, app.config['DB_SERVER_URI'], cursor_factory=DictCursor)
    
    return app


def db_connection():
    """Pushing db to app context for each request."""
    if "db_connection" not in g:
            #conn = engine.connect(current_app.config['DB_SERVER_URI'], cursor_factory=DictCursor)
            g.db_connection = current_app.db_connection_pool.getconn()
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
        current_app.db_connection_pool.putconn(db_connection)
        current_app.logger.info("Closing db connection")

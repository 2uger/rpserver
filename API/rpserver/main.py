import logging

from flask import Flask, current_app, Blueprint, g, make_response
from werkzeug.exceptions import BadRequest, InternalServerError, HTTPException
import psycopg2 as engine


from rpserver.api.config.config import BaseConfiguration

from rpserver.api.middleware.token_auth import jwt_token_authorization
from rpserver.api.middleware.exception import http_exception, internal_server_error


def auth_app(config_class=BaseConfiguration):
    """ Init app object for auth server """
    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creatin auth server")

    app.register_error_handler(InternalServerError, internal_server_error)

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    
    from rpserver.auth.auth import auth_bp
    app.register_blueprint(auth_bp, url_prefix='/auth')

    return app


def backend_app(config_class=BaseConfiguration):
    """ Initialize main app object """

    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creating main FLASK object")

    
    app.register_error_handler(HTTPException, http_exception)
    app.register_error_handler(InternalServerError, internal_server_error)

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    
    # from rpserver.api.handlers.user import user_bp
    from rpserver.api.handlers.spot import spot_bp
    # from rpserver.api.handlers.event import event_bp
    from rpserver.api.handlers.auth import auth_bp

    # app.register_blueprint(user_bp, url_prefix='/user')
    app.register_blueprint(spot_bp, url_prefix='/spot')
    # app.register_blueprint(event_bp, url_prefix='/event')
    app.register_blueprint(auth_bp, url_prefix='/auth')
    

    return app


def db_connection():
    """ Pushing db to app context for each request """

    if "db_connection" not in g:
            conn = engine.connect(current_app.config.get("DB_SERVER_URI"))
            g.db_connection = conn
            current_app.logger.info("Creating connection to db")


def shutdown_session(exception=None):
    """ Calls at the end of all requests """
    
    db_connection = g.pop("db_connection", None)
    if db_connection is not None:
        # If error occure, exception will rollback transaction
        # This commit has no effect
        db_connection.commit()
        db_connection.close()
        current_app.logger.info("Closing db connection")





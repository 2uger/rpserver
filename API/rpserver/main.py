import logging

from flask import Flask, current_app, Blueprint, g, make_response
from werkzeug.exceptions import BadRequest, InternalServerError, HTTPException, NotFound, MethodNotAllowed
from psycopg2 import DatabaseError
from psycopg2.errors import UniqueViolation
import psycopg2 as engine


from rpserver.config.config import BaseConfiguration

from rpserver.api.middleware.token_auth import jwt_token_authorization
from rpserver.api.exception import (unique_violation, base_exception, 
                                    invalid_transaction, http_exception, 
                                    internal_server_error, method_not_allowed)


logging.basicConfig(level=logging.DEBUG)


def auth_app(config_class=BaseConfiguration):
    """ Init app object for auth server """
    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creatin auth server")

    #app.register_error_handler(InternalServerError, internal_server_error)
    #app.register_error_handler(DatabaseError, invalid_transaction)
    #app.register_error_handler(Exception, base_exception)

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]
    
    from rpserver.auth.handlers import auth_bp
    app.register_blueprint(auth_bp, url_prefix='/auth')

    return app


def backend_app(config_class=BaseConfiguration):
    """ Initialize main app object """

    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info("Creating main FLASK object")

    
    app.register_error_handler(InternalServerError, internal_server_error)
    app.register_error_handler(HTTPException, http_exception)
    app.register_error_handler(DatabaseError, invalid_transaction)
    app.register_error_handler(UniqueViolation, unique_violation)
    app.register_error_handler(Exception, base_exception)
<<<<<<< HEAD
=======
    app.register_error_handler(MethodNotAllowed, method_not_allowed)
>>>>>>> 5cf738e3040769c1d0efc0f40c2c944545ce4d7b

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    
    from rpserver.api.handlers.rider import rider_bp
    from rpserver.api.handlers.spot import spot_bp
    from rpserver.api.handlers.event import event_bp

    app.register_blueprint(rider_bp, url_prefix='/rider')
    app.register_blueprint(spot_bp, url_prefix='/spot')
    app.register_blueprint(event_bp, url_prefix='/event')
    

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





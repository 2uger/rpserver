"""
Create_app() function to initialize main app
-add configuration
-init blueprints
"""


from flask import Flask, Blueprint, g

from rpserver.db import init_db, engine

from rpserver.api.config import Configuration

from rpserver.api.middleware.middleware import jwt_token_authorization
from rpserver.api.middleware.exception import (exception_list,
                                               handle_exception,
                                               internal_server_error)


def create_app(config_class=Configuration):
    app = Flask(__name__)

    app.config.from_object(Configuration)
    app.before_request_funcs = jwt_token_authorization
    app.teardown_appcontext_funcs = shutdown_session

    for i, exception in enumerate(exception_list):
        app.register_error_handler(exception, handle_exception[i])

    app.handle_exception(internal_server_error)

    from rpserver.api.handlers.user import user_bp
    from rpserver.api.handlers.spot import spot_bp
    from rpserver.api.handlers.event import event_bp
    from rpserver.api.handlers.auth import auth_bp

    app.register_blueprint(user_bp, url_prefix='/user')
    app.register_blueprint(spot_bp, url_prefix='/spot')
    app.register_blueprint(event_bp, url_prefix='/event')
    app.register_blueprint(auth_bp, url_prefix='/auth')
    
    #Initialize database with metadata
    init_db()

    return app


def shutdown_session(exception=None):
    db = g.pop('_database', None)
    if db is not None:
        db.close()


def db_connection():
    """
    Pushing db to app context for each request
    """

    if 'database' not in g:
        g.db = engine.connect()
    return g.db



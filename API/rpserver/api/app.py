"""
Create_app() function to initialize main app
-add configuration
-init blueprints
"""


from flask import Flask, Blueprint, g

from API.rpserver.db import init_db, engine

from API.rpserver.api.config import Configuration

from API.rpserver.api.utils.middleware import jwt_token_authorization
from API.rpserver.api.utils.exception import exception_list


def create_app(config_class=Configuration):
    app = Flask(__name__)

    app.config.from_object(Configuration)
    app.before_request_funcs = jwt_token_authorization
    app.teardown_appcontext_funcs = shutdown_session

    for i, exception in enumerate(exception_list):
        app.register_error_handler(exception, handle_exception[i])

    from API.rpserver.api.handlers.user import user_bp
    from API.rpserver.api.handlers.spot import spot_bp
    from API.rpserver.api.handlers.event import event_bp
    from API.rpserver.api.handlers.auth import auth_bp

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



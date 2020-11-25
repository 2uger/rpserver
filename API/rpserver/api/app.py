from flask import Flask, Blueprint, g


from rpserver.api.config.config import BaseConfiguration

from rpserver.api.middleware.token_auth import jwt_token_authorization
from rpserver.api.middleware.exception import exception_handlers


logger = logging.getLogger(__name__)


def create_app(config_class=BaseConfiguration):
    """Initialize main app object"""

    app = Flask(__name__)

    app.config.from_object(config_class)
    app.before_request_funcs = {None: jwt_token_authorization,
                                None: db_connection},
    app.teardown_appcontext_funcs = [shutdown_session]

    for exception in exception_handlers.keys:
        app.register_error_handler(exception, exception_handlers[exception])
    
    from rpserver.api.handlers.user import user_bp
    from rpserver.api.handlers.spot import spot_bp
    from rpserver.api.handlers.event import event_bp
    from rpserver.api.handlers.auth import auth_bp

    # app.register_blueprint(user_bp, url_prefix='/user')
    # app.register_blueprint(spot_bp, url_prefix='/spot')
    app.register_blueprint(event_bp, url_prefix='/event')
    app.register_blueprint(auth_bp, url_prefix='/auth')
    
    # Initialize database with metadata
    metadata.create_all(engine)

    return app


def shutdown_session(exception=None):
    db = g.pop('_database', None)
    if db is not None:
        db.close()


def db_connection():
    """ Pushing db to app context for each request """

    if 'database' not in g:
        g.db = engine.connect()
    return g.db



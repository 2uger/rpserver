from flask import Response, Flask, Blueprint, g
from werkzeug.exceptions import HTTPException

from rpserver.db import engine
from rpserver.db.schema import metadata

from rpserver.api.config.config import BaseConfiguration

from rpserver.api.middleware.middleware import token_authorization
from rpserver.api.middleware.exception import exception_handlers


def create_app(config_class=BaseConfiguration):
    """Initialize main app object"""

    app = Flask(__name__)

    app.config.from_object(config_class)
    app.before_request_funcs = {
                                None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    app.json_encoder.default = proxy_object

    for exception in exception_handlers.keys():
        app.register_error_handler(exception, exception_handlers[exception])

    
    from rpserver.api.handlers.user import user_bp
    from rpserver.api.handlers.spot import spot_bp
    from rpserver.api.handlers.event import event_bp
    from rpserver.api.handlers.auth import auth_bp

    app.register_blueprint(user_bp, url_prefix='/user')
    app.register_blueprint(spot_bp, url_prefix='/spot')
    app.register_blueprint(event_bp, url_prefix='/event')
    app.register_blueprint(auth_bp, url_prefix='/auth')
    
    # Initialize database with metadata
    metadata.create_all(engine)

    for rule in app.url_map.iter_rules():
        print(rule)
    return app


def shutdown_session(exception=None):
    print("Close db_connection")
    print(g.snt)
    if g.db_connection is not None:
        g.db_connection.close()



def db_connection():
    """
    Pushing db to app context for each request
    """
    g.snt = 'Olegf'

    if '_database' not in g:
        g.db_connection = engine.connect()


# Serialize result from sqlalchemy
def proxy_object(self, result):
    try:
        resp = dict(o)
    except Exception:
        return {}
    else:
        return resp

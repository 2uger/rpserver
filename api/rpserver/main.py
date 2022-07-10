from flask import Flask, current_app, g
from flask_cors import CORS
from psycopg2.extras import DictCursor
import psycopg2 as engine

from rpserver.auth.token_auth import token_auth

def auth_app(config_class):
    """ Init app object for auth server."""
    app = Flask(__name__)
    CORS(app)
    app.config.from_object(config_class)
    app.logger.info(f'Creating auth server app with {app.config["ENV"]}')

    app.before_request_funcs = {None: [db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    from rpserver.auth import auth_bp
    app.register_blueprint(auth_bp, url_prefix='/')

    return app

def api_app(config_class):
    """ Initialize main app object."""
    app = Flask(__name__)
    app.config.from_object(config_class)
    app.logger.info(f'Creating API server with {app.config["ENV"]}')

    CORS(app)
    app.before_request_funcs = {None: [token_auth, db_connection]}
    app.teardown_appcontext_funcs = [shutdown_session]

    from rpserver.rider import rider_bp
    from rpserver.spot import spot_bp
    from rpserver.event import event_bp

    app.register_blueprint(rider_bp, url_prefix='/riders')
    app.register_blueprint(spot_bp, url_prefix='/spots')
    app.register_blueprint(event_bp, url_prefix='/events')

    return app

def db_connection():
    """Pushing db to app context for each request."""
    if 'db_connection' not in g:
            conn = engine.connect(current_app.config['DB_SERVER_URI'], cursor_factory=DictCursor)
            g.db_connection = conn
            current_app.logger.info('Creating connection to db')

def shutdown_session(exception=None):
    """Calls at the end of all requests."""
    db_connection = g.pop("db_connection", None)
    if db_connection:
        # If error occure, exception will rollback transaction
        # This commit has no effect
        db_connection.commit()
        current_app.logger.info('Commit to database')
        db_connection.close()
        current_app.logger.info('Closing db connection')
    if exception:
        raise exception

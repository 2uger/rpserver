from flask import Flask, Blueprint


from ridersPlatform.api.config import Configuration
from ridersPlatform.api.utils.requests import JSONrequest
from ridersPlatform.db import init_db


def create_app(config_class=Configuration):
    app = Flask(__name__)
    app.config.from_object(Configuration)
    app.request_class = JSONrequest

    from ridersPlatform.api.blueprints.user import user_bp
    from ridersPlatform.api.blueprints.spot import spot_bp
    from ridersPlatform.api.blueprints.event import event_bp
    app.register_blueprint(user_bp, url_prefix='/user')
    app.register_blueprint(spot_bp, url_prefix='/spot')
    app.register_blueprint(event_bp, url_prefix='/event')
    
    #Initialize database with metadata
    init_db()

    return app


@app.teardown_appcontext
def shutdown_session(exception=None):
    db_session.remove()




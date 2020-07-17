from flask import Flask, Blueprint


from ridersPlatform.api.config import Configuration
from ridersPlatform.api.requests import JSONrequest


from sqlalchemy import create_engine
from sqlalchemy.orm import scoped_session, sessionmaker


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

    return app




from flask import Flask, Blueprint

from ridersPlatform.api.config import Configuration


def create_app(config_class=Configuration):
    app = Flask(__name__)
    app.config.from_object(Configuration)

    from ridersPlatform.api.blueprints.user import user_bp
    from ridersPlatform.api.blueprints.spot import spot_bp
    app.register_blueprint(user_bp, url_prefix='/user')
    app.register_blueprint(spot_bp, url_prefix='/spot')

    return app




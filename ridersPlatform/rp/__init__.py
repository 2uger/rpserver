from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_login import LoginManager
from flask_migrate import Migrate
from flask_socketio import SocketIO
from flask_cors import CORS, cross_origin


from ridersPlatform.config import Configuration


db = SQLAlchemy()
migrate = Migrate()
socketio = SocketIO(cors_allowed_origins='*')


def create_app(config_class=Configuration):
    app = Flask(__name__)
    app.config.from_object(Configuration)

    db.init_app(app)
    migrate.init_app(app=app, db=db)
    socketio.init_app(app)

    from ridersPlatform.coordinates import coordinates_bp
    from ridersPlatform.rider import rider_bp
    from ridersPlatform.spot import spot_bp
    app.register_blueprint(coordinates_bp)
    app.register_blueprint(rider_bp, url_prefix='/rider')
    app.register_blueprint(spot_bp, url_prefix='/spot')

    return app




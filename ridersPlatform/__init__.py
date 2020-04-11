from flask import Flask
from ridersPlatform.config import Configuration
from flask_sqlalchemy import SQLAlchemy
from flask_login import LoginManager
from flask_migrate import Migrate



db = SQLAlchemy()
login_manager = LoginManager()

#getting control of version App and database
migrate = Migrate()




def create_app(config_class=Configuration):
    app = Flask(__name__)
    app.config.from_object(Configuration)

    db.init_app(app)
    login_manager.init_app(app)
    migrate.init_app(app=app, db=db)

    from ridersPlatform.rider.routes import rider
    from ridersPlatform.spot.routes import spot
    from ridersPlatform.main.routes import main
    app.register_blueprint(rider, url_prefix='/rider')
    app.register_blueprint(spot, url_prefix='/spot')
    app.register_blueprint(main)

    return app




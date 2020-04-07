from flask import Flask
from config import Configuration
from flask_sqlalchemy import SQLAlchemy

from flask_migrate import Migrate, MigrateCommand
from flask_script import Manager

app = Flask(__name__)
app.config.from_object(Configuration)

db = SQLAlchemy(app)

#getting control of version App and database
migrate = Migrate(app, db)

manager = Manager(app)
#Commamd that we will use to make migrations
manager.add_command('db', MigrateCommand)


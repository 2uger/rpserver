from flask import current_app
from werkzeug.security import generate_password_hash, check_password_hash

from ridersPlatform import db, login_manager


@login_manager.user_loader
def load_rider(user_id):
    return Rider.query.get(int(user_id))


class Rider(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100), nullable=False)
    surname = db.Column(db.String(100), nullable=False)
    age = db.Column(db.Integer, nullable=False)
    login_email = db.Column(db.String(100), nullable=False, unique=True)
    password = db.Column(db.String(100), nullable=False)
    hometown = db.Column(db.String(100), nullable=False)
    profile_image = db.Column(db.String(100), unique=True, default='default.jpg')
    remember_me = db.Column(db.Boolean, )

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.set_password(self.password)

    def __repr__(self):
        rider_information = {
            'name': self.name,
            'surname': self.surname,
            'age': self.age,
            'login_email': self.login_email,
            'hometown': self.hometown,
        }
        return f'{rider_information}'

    def set_password(self, password):
        self.password = generate_password_hash(password)

    def check_password(self, password):
        return check_password_hash(self.password, password)

    def to_dict(self):
        rider_information = {
            'name': self.name,
            'surname': self.surname,
            'age': self.age,
            'login_email': self.login_email,
            'hometown': self.hometown,
        }
        return rider_information


class Spot(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(50), nullable=False)
    profile_image = db.Column(db.String(50), nullable=False, unique=True)
    location = db.Column(db.String(100), nullable=False, unique=True)
    notes = db.Column(
        db.String(200),
        nullable=False,
        unique=True,
        default='There are no notes about this spot'
    )

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __repr__(self):
        return f"Spot {self.spot_name}"

    def to_dict(self):
        spot_information = {
            'name': self.name,
            'location': self.location,
            'notes': self.notes
        }
        return spot_information

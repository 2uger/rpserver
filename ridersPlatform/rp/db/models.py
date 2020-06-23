from flask import current_app
from werkzeug.security import generate_password_hash, check_password_hash
from datetime import datetime, timedelta
import base64
import os


from ridersPlatform import db


class Rider(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100), nullable=False)
    surname = db.Column(db.String(100), nullable=False)
    age = db.Column(db.Integer)
    login_email = db.Column(db.String(100), nullable=False)
    password = db.Column(db.String(100), nullable=False)
    hometown = db.Column(db.String(100), nullable=False)
    profile_image = db.Column(db.String(100), default='default.jpg')
    remember_me = db.Column(db.Boolean, default=False)
    token = db.Column(db.String, index=True, unique=True)
    token_expiration = db.Column(db.DateTime)
    friendship_request = db.Column(db.String(1000), default='')
    friends_id = db.Column(db.String(1000), default='')
    coordinates = db.relationship('Coordinates', backref='rider', uselist=False)


    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

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

    def get_token(self, expires_in=3600*24):
        now = datetime.utcnow()
        if self.token and self.token_expiration > now + timedelta(seconds=60):
            return self.token
        self.token = base64.b64encode(os.urandom(23)).decode('utf-8')
        self.token_expiration = now + timedelta(seconds=expires_in)
        db.session.add(self)
        return self.token

    def revoke_token(self):
        self.token_expiration = datetime.utcnow() - timedelta(seconds=1)

    @staticmethod
    def user_by_token(token):
        rider = Rider.query.filter(Rider.token == token).first()
        if not rider or rider.token_expiration < datetime.utcnow():
            return None
        return rider

    def to_dict(self):
        rider_information = {
            'name': self.name,
            'surname': self.surname,
            'age': self.age,
            'login_email': self.login_email,
            'hometown': self.hometown,
        }
        return rider_information

    def from_dict(self, rider_information):
        for attr in rider_information:
            if attr == 'password':
                self.set_password(rider_information[attr])
                continue
            setattr(self, attr, rider_information[attr])

    @classmethod
    def add_to_db(cls, self):
        db.session.add(self)
        db.session.commit()

    @classmethod
    def delete_from_db(cls, self):
        db.session.delete(self)
        db.session.commit()


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

    def from_dict(self, spot_information):
        for attr in spot_information:
            setattr(self, attr, spot_information[attr])
        return self

    @classmethod
    def add_to_db(cls, self):
        db.session.add(self)
        db.session.commit()

    @classmethod
    def delete_from_db(cls, self):
        db.session.delete(self)
        db.session.commit()


class Coordinates(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    longitude = db.Column(db.Integer, nullable=False, default=0)
    latitude = db.Column(db.Integer, nullable=False, default=0)
    rider_id = db.Column(db.Integer, db.ForeignKey('rider.id'))

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __repr__(self):
        return f'{self.rider_id} : {self.coordinate}'

    def from_dict(self, coordinates):
        self.latitude = coordinates['coordinates'][1]
        self.longitude = coordinates['coordinates'][0]

    def to_dict(self):
        coordinates_dict = {
            'rider.id': self.rider_id,
            'coordinates': tuple(self.latitude, self.longitude)
        }
        return coordinates_dict

    @classmethod
    def add_to_db(cls, self):
        db.session.add(self)
        db.session.commit()

    @classmethod
    def delete_from_db(cls, self):
        db.session.delete(self)
        db.session.commit()


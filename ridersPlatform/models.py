from flask import current_app

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
    hometown = db.Column(db.String(100), nullable=False, unique=True)
    profile_image = db.Column(db.String(100), nullable=False, unique=True, default='default.jpg')
    remember_me = db.Column(db.Boolean, )
    spot_id = db.Column(db.Integer, db.ForeignKey('spot.id'))


    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __repr__(self):
        return f"Rider ({self.name}, {self.login}, {self.hometown})"


class Spot(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(50), nullable=False)
    profile_image = db.Column(db.String(50), nullable=False, unique=True)
    location = db.Column(db.String(100), nullable=False, unique=True)
    notes = db.Column(db.String(200), nullable=False, unique=True, default="There is not notes about this spot")
    riders_on_spot = db.relationship('Rider', backref="spot", lazy=True)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __repr__(self):
        return f"Spot {self.spot_name}"

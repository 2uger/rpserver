from app import db

class Rider(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100), nullable=False)
    surname = db.Column(db.String(100), nullable=False)
    age = db.Column(db.Integer, nullable=False)
    login = db.Column(db.String(100), nullable=False, unique=True)
    password = db.Column(db.String(100), nullable=False)
    hometown = db.Column(db.String(100), nullable=False, unique=True)
    profile_image = db.Column(db.String(100), nullable=False, unique=True) 
    spot_id = db.Column(db.Integer, db.ForeignKey('spot.id'))


    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __repr__(self):
        return f"Rider {self.name}, {selg.login}, {self.hometown}" 

class Spot(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    spot_name = db.Column(db.String(50), nullable=False)
    spot_profile_image = db.Column(db.String(50), nullable=False, unique=True)
    riders_on_spot = db.relationship('Rider', backref="spot", lazy=True)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __repr__(self):
        return f"Spot {self.spot_name}"

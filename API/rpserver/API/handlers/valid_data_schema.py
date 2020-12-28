"""
Schemas to check out user request data
"""


from datetime import date


from marshmallow import Schema, validates, ValidationError
from marshmallow.fields import Str, Int, Float, Date
from marshmallow.validate import Length, OneOf, Range


class LoginUserSchema(Schema):
    name = Str(validate=Length(min=1, max=50), required=True)
    login_email = Str(validate=Length(min=1, max=50), required=True)


class PostUserSchema(Schema):
    name = Str(validate=Length(min=1, max=50), required=True)
    surname = Str(validate=Length(min=1, max=50), required=True)
    login_email = Str(validate=Length(min=1, max=50), required=True)
    password = Str(validate=Length(min=8), required=True)
    birth_date = Date(format=BIRTH_DATE_FORMAT, required=True)
    bio = Str(validate=Length(max=200))
    hometown = Str(validate=Length(min=2, max=50), required=True)

    @validates('birth_date')
    def validate_birth_date(self, value: date):
        if value > date.today():
            raise ValidationError('Birth date cant be higher than todays date')


class PostSpotSchema(Schema):
    location = Str(validate=Length(min=2, max=50))
    notes = Str(validate=Length(min=0, max=100))


class PostEventSchema(Schema):
    user_id = Int()
    spot_id = Int()
    name = Str(validate=Length(min=3, max=20))
    description = Str(validate=Length(min=5, max=40))

    @validates('user_id')
    def validate_user_id(self, value):
        if value <= 0:
            raise ValidationError('User id can not be lower than 0')
            
    @validates('spot_id')
    def validate_spot_id(self, value):
        if value <= 0:
            raise ValidationError('Spot id can not be lower than 0')
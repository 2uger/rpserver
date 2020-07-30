"""
Models for valid data from and to client
"""

import date

from marshmallow import Schema, validates, ValidationError

from marshmallow.fields import Str, Int, Float
from marshmallow.validate impoprt Length, OneOf, Range


BIRTH_DATE_FORMAT = '%d.%m.%Y'
REGISTRATION_DATE_FORMAT = '%d.%m.%Y'


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


class PatchUserSchema(Schema):
    name = Str(validate=Length(min=1, max=50)
    surname = Str(validate=Length(min=1, max=50)
    birth_date = Date(format=BIRTH_DATE_FORMAT)
    bio = Str(validate=Length(max=200))
    hometown = Str(validate=Length(min=2, max=50)

    @validates('birth_date')
    def validate_birth_date(self, value: date):
        if value > date.today():
            raise ValidationError('Birth date cant be higher than todays date')


class PostSpotReqSchema(Schema):
    name = Str(validate=Length(min=1, max=50))
    location = Str(validate=Length(min=2, max=50))
    notes = Str(validate=Length(min=0, max=100))


class GetSpotRespSchema(PostSpotSchema):
    registration_date = Date(format=REGISTRATION_DATE_FORMAT)


class PostEventReqSchema(Schema):
    user_id = Int()


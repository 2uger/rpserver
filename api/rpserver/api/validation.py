from datetime import date

from marshmallow import Schema, validates, ValidationError, fields
from marshmallow.validate import Length, OneOf, Range


class LoginUserSchema(Schema):
    name = fields.Str(validate=Length(min=1, max=50), required=True)
    login_email = fields.Str(validate=Length(min=1, max=50), required=True)


class PostUserSchema(Schema):
    name = fields.Str(validate=Length(min=1, max=50), required=True)
    surname = fields.Str(validate=Length(min=1, max=50), required=True)
    login_email = fields.Str(validate=Length(min=1, max=50), required=True)
    password = fields.Str(validate=Length(min=8), required=True)
    bio = fields.Str(validate=Length(max=200))
    hometown = fields.Str(validate=Length(min=2, max=50), required=True)

    @validates('birth_date')
    def validate_birth_date(self, value: date):
        if value > date.today():
            raise ValidationError('Birth date cant be higher than todays date')


class PostSpotSchema(Schema):
    title = fields.Str(validate=Length(min=2, max=50), required=True)
    coordinates = fields.Tuple((fields.Float(), fields.Float()), required=True)
    notes = fields.Str(validate=Length(min=0, max=400), required=True)
    profile_image_url = fields.Str(validate=Length(min=0, max=400), required=True)


class PatchSpotSchema(Schema):
    title = fields.Str(validate=Length(min=2, max=50))
    coordinates = fields.Tuple((fields.Float(), fields.Float()))
    notes = fields.Str(validate=Length(min=0, max=400))
    profile_image_url = fields.Str(validate=Length(min=0, max=400))
    

class PostEventSchema(Schema):
    rider_id = fields.Int(required=True)
    spot_id = fields.Int(required=True)
    title = fields.Str(validate=Length(min=3, max=50), required=True)
    description = fields.Str(validate=Length(min=5, max=400), required=True)
    when_date = fields.DateTime(required=True)


class PatchEventSchema(Schema):
    user_id = fields.Int()
    spot_id = fields.Int()
    title = fields.Str(validate=Length(min=3, max=50))
    description = fields.Str(validate=Length(min=5, max=400))
    when_date = fields.DateTime()

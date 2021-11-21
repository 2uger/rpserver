from datetime import date

from marshmallow import Schema, fields
from marshmallow.validate import Length


class SpotSchema(Schema):
    title = fields.Str(validate=Length(min=2, max=50), required=True)
    coordinates = fields.Tuple((fields.Float(), fields.Float()), required=True)
    notes = fields.Str(validate=Length(min=0, max=400), required=True)
    profile_image_url = fields.Str(validate=Length(min=0, max=400), required=True)


class UpdateSpotSchema(Schema):
    title = fields.Str(validate=Length(min=2, max=50))
    coordinates = fields.Tuple((fields.Float(), fields.Float()))
    notes = fields.Str(validate=Length(min=0, max=400))
    profile_image_url = fields.Str(validate=Length(min=0, max=400))

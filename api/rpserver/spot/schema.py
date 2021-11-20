from datetime import date

from marshmallow import Schema, validates, ValidationError, fields, post_load
from marshmallow.validate import Length, OneOf, Range

from .model import Spot


class SpotSchema(Schema):
    title = fields.Str(validate=Length(min=2, max=50), required=True)
    coordinates = fields.Tuple((fields.Float(), fields.Float()), required=True)
    notes = fields.Str(validate=Length(min=0, max=400), required=True)
    profile_image_url = fields.Str(validate=Length(min=0, max=400), required=True)


class PatchSpotSchema(Schema):
    title = fields.Str(validate=Length(min=2, max=50))
    coordinates = fields.Tuple((fields.Float(), fields.Float()))
    notes = fields.Str(validate=Length(min=0, max=400))
    profile_image_url = fields.Str(validate=Length(min=0, max=400))

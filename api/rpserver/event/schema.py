from datetime import date

from marshmallow import Schema, fields
from marshmallow.validate import Length


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

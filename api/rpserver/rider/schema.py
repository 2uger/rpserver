from datetime import date

from marshmallow import Schema, validates, ValidationError, fields, post_load
from marshmallow.validate import Length, OneOf, Range


class LoginUserSchema(Schema):
    password = fields.Str(validate=Length(min=8), required=True)


class PostUserSchema(Schema):
    nickname = fields.Str(validate=Length(min=1, max=50), required=True)
    password = fields.Str(validate=Length(min=8), required=True)

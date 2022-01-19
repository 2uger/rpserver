from datetime import date

from marshmallow import Schema, fields
from marshmallow.validate import Length


class LoginUserSchema(Schema):
    password = fields.Str(validate=Length(min=8), required=True)


class PostUserSchema(Schema):
    nickname = fields.Str(validate=Length(min=1, max=50), required=True)
    password = fields.Str(validate=Length(min=8), required=True)


class PatchUserSchema(Schema):
    nickname = fields.Str(validate=Length(min=1, max=50), required=False)
    bio = fields.Str(validate=Length(min=10), required=False)
    hometown = fields.Str(validate=Length(min=3), required=False)
    

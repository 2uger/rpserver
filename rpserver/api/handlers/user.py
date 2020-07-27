"""
Handler for User 
"""


from marshmallow.exc import ValidationError


from .handler import Handler 

from rpserver.db import engine
from rpserver.db.schema import user_table

from .validator_schemas import PostUserSchema, PatchUserSchema


class UserHandler:
    def __init__(self):
        pass
        
    @staticmethod
    def add_user(request):
        try:
            with engine.connect() as connection:
                response = Handler.post(request, PostUserSchema, connection, user_table)
        except ValidationError as ve:
            #make logging
            return 400, response
        else:
            return 200, response
    
    @staticmethod
    def get_user(user_id):
        with engine.connect() as connection:
            response = Handler.get(connection, user_table, user_id)
            return 200, response

    @staticmethod
    def update_user(user_id, request):
        with engine.connect() as connection:
            response = Handler.patch(request, PatchUserSchema, connection, user_table, user_id)
            return 200, ''

    @staticmethod
    def delete_user(user_id):
        with engine.connect() as connection:
            response = Handler.delete(connection, user_table, user_id)
            return 200, ''


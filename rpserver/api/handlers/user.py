"""
Handlers for User 
"""


from .handler import BaseHandler 

from rpserver.db.schema import user_table, user_relation



class UserHandler(BaseHandler):
    def __init__(self):
        pass
        
    @staticmethod
    def add_user(connection, user_data):
        return super(UserHandler, self).post(connection, user_table, user_data)
    
    @staticmethod
    def get_user(connection, user_id):
        return super(UserHandler, self).get(connection, user_table, user_id)

    @staticmethod
    def update_user(connection, user_id, user_update_data):
        return super(UserHandler).patch(connection, user_id, user_update_data)

    @staticmethod
    def delete_user(connection, user_id):
        return super(UserHandler, self).delete(connection, user_id)


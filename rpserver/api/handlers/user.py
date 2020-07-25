"""
Handler for User 
"""


from .handler import BaseHandler 

from rpserver.db.schema import user_table, user_relation



class UserHandler(BaseHandler):
    def __init__(self):
        pass
        
    @staticmethod
    def add_user(user_data):
        return super(UserHandler, self).post(user_table, user_data)
    
    @staticmethod
    def get_user(user_id):
        return super(UserHandler, self).get(user_table, user_id)

    @staticmethod
    def update_user(user_id, user_update_data):
        return super(UserHandler).patch(user_id, user_update_data)

    @staticmethod
    def delete_user(user_id):
        return super(UserHandler, self).delete(user_id)


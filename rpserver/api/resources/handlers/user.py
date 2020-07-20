"""
Handlers for User 
"""


from .handler import BaseHandler 


class UserHandler(BaseHandler):
    def __init__(self, request):
        super(UserHandler, self).__init__(request)

    @staticmethod
    def add_user(connection, user_data):
        pass
    
    @staticmethod
    def get_user(connection, user_id):
        pass

    @staticmethod
    def update_user(connection, user_update_data):
        pass

    @staticmethod
    def add_user(connection, user_data):
        pass

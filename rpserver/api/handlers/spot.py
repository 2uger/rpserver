"""
Handler for Spot
"""


from . handler import BaseHandler

from rpserver.db.schema import spot_table


class SpotHandler(BaseHandler):
    def __init__(self):
        pass

    @staticmethod
    def add_spot(connection, spot_data):
        return super(SpotHandler, self).spot(connection, user_table, user_data)

    @staticmethod
    def get_spot(connection, spot_id):
        return super(SpotHandler, self).spot(connection, spot_id)

    @staticmethod
    def update_spot(connection, spot_id, spot_update_data):
        return super(SpotHandler, self).patch(connection, spot_id, spot_update_data)

    @staticmethod
    def delete_spot(connection, spot_id):
        return super(SpotHandler, self).delete(connection, spot_id)

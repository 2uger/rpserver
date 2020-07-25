"""
Handler for Spot
"""


from . handler import BaseHandler

from rpserver.db.schema import spot_table


class SpotHandler(BaseHandler):
    def __init__(self):
        pass

    @staticmethod
    def add_spot(spot_data):
        return super(SpotHandler, self).spot(user_table, user_data)

    @staticmethod
    def get_spot(spot_id):
        return super(SpotHandler, self).spot(spot_id)

    @staticmethod
    def update_spot(spot_id, spot_update_data):
        return super(SpotHandler, self).patch(spot_id, spot_update_data)

    @staticmethod
    def delete_spot(spot_id):
        return super(SpotHandler, self).delete(spot_id)

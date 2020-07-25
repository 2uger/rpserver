"""
Handler for event
"""


from rpserver.db.schema import event_table

from .handler import BaseHandler


class EventHandler(BaseHandler):
    def __init__(self):
        pass

    @staticmethod
    def add_event(event_data):
        super(EventHandler, self).post(event_table)

    @staticmethod
    def get_event(event_id):
        return super(EventHandler, self).event(event_id)

    @staticmethod
    def update_event(event_id, event_update_data):
        return super(EventHandler, self).patch(event_id, event_update_data)

    @staticmethod
    def delete_event(event_id):
        return super(EventHandler, self).delete(event_id)

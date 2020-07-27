"""
API requests for event on map
"""


from flask import request, make_response

from . import event_bp

from rpserver.api.handlers import EventHandler
from rpserver.api.utils.response import JSONResponse 


@event_bp.route('/add/', methods=['POST'])
def add_event():
    status, response = EventHandler.add_event(request)
    status_depend_response(status, response)


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    status, response = EventHandler.get_event(event_id)
    status_depend_response(status, response)


@event_bp.route('/update/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    status, response = EventHandler.update_event(request, event_id)
    status_depend_response(status, response)


@event_bp.route('/delete/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    status, response = EventHandler.delete_event(event_id)
    status_depend_response(status, response)

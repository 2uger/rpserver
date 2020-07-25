"""
API requests for event on map
"""


from flask import request, make_response

from . import event_bp

from rpserver.api.handlers import EventHandler
from rpserver.api.utils.response import JSONResponse 


@event_bp.route('/add/', methods=['POST'])
def register_event():
    event_data= request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(JSONResponse(status_message='Not valid JSON fields',
                                          status_code=400,
                                          response={}))
    with engine.connect() as connection:
        if EventHandler.add_event(connection, event_data):
            return make_response(JSONResponse(status_message='Event has been added',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='Event has NOT been added',
                                              status_code=400,
                                              response={}))


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    if event_id < 0:
        return make_response(JSONResponse(status_message='Invalid event_id', 
                                          status_code=400, 
                                          response={}))
    with engine.connect() as connection:
        response = UserHandler.get_event(connection, event_id)
    if response:
        return make_response(JSONResponse(status_message='Event data', 
                                          status_code=200, 
                                          response=response))
    else:
        return make_response(JSONResponse(status_message='No such event', 
                                          status_code=400, 
                                          response={}))


@event_bp.route('/update/<int:event_id>', methods=['PATCH'])
def update_event(event_id):
    event_update_data = request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(JSONResponse(status_message='Not valid JSON fileds',
                                          status_code=400,
                                          response={}))
    with engine.connect() as connection:
        if EventHandler.update_event(connection, event_id, event_update_data):
            return make_response(JSONResponse(status_message='Spot data has been updated',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='Event has NOT been updated',
                                              status_code=400,
                                              response={}))


@event_bp.route('/delete/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    if event_id < 0:
        return make_response(JSONResponse(status_message='Invalid event_id',
                                          status_code=400,
                                          respones={}))
    with engine.connect() as connection:
        if EventHandler.delete_event(connection, event_id):
            return make_response(JSONResponse(status_message='Event has been deleted',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='No such event to delete',
                                              status_code=400,
                                              response={}))

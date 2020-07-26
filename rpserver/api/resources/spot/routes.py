"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, current_app

from . import spot_bp

from rpserver.api.handlers import SpotHandler
from rpserver.api.utils import JSONResponse


@spot_bp.route('/add/', methods=['POST'])
def register_spot():
    spot_data = request.get_json()
    try:
        response = PostSpotSchema().load(spot_data)
    except ValidationError ass ve:
        make_response(ErrorResponse(err_message=ve.messages))
    else:
        status, response = SpotHandler.add_spot(spot_data)
        if status:
            make_response(JsonResponse(response=response))
        else:
            make_response(ErrorResponse(err_message=response))
#
#
#REWRITE AFTER TESTING USER ROUTES
#
#
#############################################################
@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    if spot_id < 0:
        return make_response(JSONResponse(status_message='Invalid spot_id', 
                                          status_code=400, 
                                          response={}))
    with engine.connect() as connection:
        response = SpotHandler.get_spot(connection, spot_id)
    if response:
        return make_response(JSONResponse(status_message='Spot data', 
                                          status_code=200, 
                                          response=response))
    else:
        return make_response(JSONResponse(status_message='No such spot', 
                                          status_code=400, 
                                          response={}))


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    spot_update_data = request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(JSONResponse(status_message='Not valid JSON fileds',
                                          status_code=400,
                                          response={}))
    with engine.connect() as connection:
        if SpotHandler.update_spot(connection, spot_id, spot_update_data):
            return make_response(JSONResponse(status_message='Spot data has been updated',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='Spot has NOT been updated',
                                              status_code=400,
                                              response={}))


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    if spot_id < 0:
        return make_response(JSONResponse(status_message='Invalid spot_id',
                                          status_code=400,
                                          respones={}))
    with engine.connect() as connection:
        if SpotHandler.delete_spot(connection, spot_id):
            return make_response(JSONResponse(status_message='Spot has been deleted',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='No such spot to delete',
                                              status_code=400,
                                              response={}))

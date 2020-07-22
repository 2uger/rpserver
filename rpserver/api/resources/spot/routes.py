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


@spot_bp.route('/register/', methods=['POST'])
def register_spot():
    spot_data = request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(Response(message='Not valid JSON fields',
                                      status=400,
                                      response={}))
    with engine.connect() as connection:
        if SpotHandler.add_spot(connection, spot_data):
            return make_response(Response(message='Spot has been added',
                                          status=200,
                                          response={}))
        else:
            return make_response(Response(message='Spot has NOT been added',
                                          status=400,
                                          response={}))
    

@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    if spot_id < 0:
        return make_response(Response(message='Invalid spot_id', 
                                      status=400, 
                                      response={}))
    with engine.connect() as connection:
        response = SpotHandler.get_spot(connection, spot_id)
    if response:
        return make_response(Response(message='Spot data', 
                                      status=200, 
                                      response=response))
    else:
        return make_response(Response(message='No such spot', 
                                      status=400, 
                                      response={}))


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    spot_update_data = request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(Response(message='Not valid JSON fileds',
                                      status=400,
                                      response={}))
    with engine.connect() as connection:
        if SpotHandler.update_spot(connection, spot_id, spot_update_data):
            return make_response(Response(message='Spot data has been updated',
                                          status=200,
                                          response={}))
        else:
            return make_response(Response(message='Spot has NOT been updated',
                                          status=400,
                                          response={}))


@spot_bp.route('/remove/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    if spot_id < 0:
        return make_response(Response(message='Invalid spot_id',
                                      status=400,
                                      respones={}))
    with engine.connect() as connection:
        if SpotHandler.delete_spot(connection, spot_id):
            return make_response(Response(message='Spot has been deleted',
                                          status=200,
                                          response={}))
        else:
            return make_response(Response(message='No such spot to delete',
                                          status=400,
                                          response={}))

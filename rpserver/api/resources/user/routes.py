"""
API requests for users
"""


from flask import request, make_response, g

from rpserver.db import engine
from rpserver.api.utils.response import JSONResponse

from ..handlers.user import UserHandler
from .import user_bp



@user_bp.route('/add/', methods=['POST'])
def register_user():
    user_data = request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(JSONResponse(status_message='', status_code=400, response={}))
    with engine.connect() as connection:
        if UserHandler.add_user(connection, user_data, status_message):
            return make_response(JSONResponse(status_status_message='User has been added', 
                                              status_code=200, 
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='User has NOT been added', 
                                              status_code=502, 
                                              response={}))


@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    if user_id < 0:
        return make_response(JSONResponse(status_message='Invalid user_id', 
                                          status_code=400, 
                                          response={}))
    with engine.connect() as connection:
        response = UserHandler.get_user(connection, user_id)
    if response:
        return make_response(JSONResponse(status_message='User data', 
                                          status_code=200, 
                                          response=response))
    else:
        return make_response(JSONResponse(status_message='No such user', 
                                          status_code=400, 
                                          response={}))


@user_bp.route('/update/<int:user_id>', methods=['PATCH'])
def update_user(user_id):
    user_update_data = request.get_json()
    ###MAKE DATA VALIDATION
        return make_response(JSONResponse(status_message='Not valid JSON fileds',
                                          status_code=400,
                                          response={}))
    with engine.connect() as connection:
        if UserHandler.update_user(connection, user_id, user_update_data):
            return make_response(JSONResponse(status_message='User data has been updated',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='User has NOT been updated',
                                              status_code=400,
                                              response={}))


@user_bp.route('/friends/<int:user_id>, methods=['GET'])
def get_friends(user_id):
    if user_id < 0:
        return make_response(JSONResponse(status_message='Invalid user_id',
                                          status_code=400,
                                          response={}))
    with engine.connect() as connection:
        response = UserHandler.get_friends(connection, user_id):
    if response:
        return make_response(JSONResponse(status_message='User's friends',
                                          status_code=200,
                                          response=response))
    else:
        return make_response(JSONResponse(status_message='No such user',
                                          status_code=400,
                                          response={}))


@user_bp.route('/remove/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    if user_id < 0:
        return make_response(JSONResponse(status_message='Invalid user_id',
                                          status_code=400,
                                          respones={}))
    with engine.connect() as connection:
        if UserHandler.delete_user(connection, user_id):
            return make_response(JSONResponse(status_message='User has been deleted',
                                              status_code=200,
                                              response={}))
        else:
            return make_response(JSONResponse(status_message='No such user to delete',
                                              status_code=400,
                                              response={}))

@user_bp.route('/friendship/send/<int:user_id>', methods=['POST'])
def send_friendship(user_id):
    pass


@user_bp.route('/friendship/accept/<int:user_id>', methods=['POST'])
def accept_friendship(user_id):
    pass


@user_bp.route('/friendship/remove/<int:user_id>', methods=['POST'])
def remove_friendship(user_id):
    pass





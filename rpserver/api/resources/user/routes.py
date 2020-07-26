"""
API requests for users
"""


from flask import request, make_response, g

from rpserver.db import engine
from rpserver.api.utils.response import JsonResponse, ErrorResponse

from ..handlers.user import UserHandler
from .import user_bp
from ..validator_schemas import PostUserSchema, PatchUserSchema


@user_bp.route('/add/', methods=['POST'])
def register_user():
    user_data = request.get_json()
    try:
        response = PostUserSchema().load(user_data)
    except ValidationError ass ve:
        make_response(ErrorResponse(err_message=ve.messages))
    else:
        status, response = UserHandler.add_user(user_data)
        if status:
            make_response(JsonResponse(response=response))
        else:
            make_response(ErrorResponse(err_message=response))


@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    if user_id < 0:
        make_response(ErrorResponse(err_message='Invalid user ID'))
    status, response = UserHandler.get_user(user_id)
    if status:
        make_response(JsonResponse(response=response))
    else:
        make_response(ErrorResponse(err_message=response))


@user_bp.route('/update/<int:user_id>', methods=['PATCH'])
def update_user(user_id):
    user_update_data = request.get_json()
    try:
        response = PatchUserSchema().load(user_update_data)
    except ValidationError as ve:
        make_response(ErrorResponse(err_message=ve.messages))
    else:
        status, response = UserHandler.update_user(user_id, user_update_data)
        if status:
            make_response(JsonResponse(response=response))
        else:
            make_response(ErrorResponse(err_message=response))


@user_bp.route('/remove/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    if user_id < 0:
        make_response(ErrorResponse(err_message='Invalid user ID'))
        status, response = UserHandler.delete_user(user_id):
            make_response(JsonResponse(response=response))
        else:
            make_response(ErrorResponse(err_message=response))


@user_bp.route('/friends/<int:user_id>, methods=['GET'])
def get_friends(user_id):
    pass


@user_bp.route('/friendship/send/<int:user_id>', methods=['POST'])
def send_friendship(user_id):
    pass


@user_bp.route('/friendship/accept/<int:user_id>', methods=['POST'])
def accept_friendship(user_id):
    pass


@user_bp.route('/friendship/remove/<int:user_id>', methods=['POST'])
def remove_friendship(user_id):
    pass





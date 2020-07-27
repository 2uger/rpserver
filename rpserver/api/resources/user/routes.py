"""
API requests for users
"""


from flask import request, g

from rpserver.api.utils.response import JsonResponse, ErrorResponse

from ..handlers.user import UserHandler
from .import user_bp
from ..routes import status_depend_response


@user_bp.route('/add/', methods=['POST'])
def register_user():
    status, response = UserHandler.add_user(request)
    status_depend_response(status, response)


@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    if user_id < 0:
        make_response(ErrorResponse(err_message='Invalid user ID'))
    status, response = UserHandler.get_user(user_id)
    status_depend_response(status, response)


@user_bp.route('/update/<int:user_id>', methods=['PATCH'])
def update_user(user_id):
    status, response = UserHandler.update_user(request)
    status_depend_response(status, response)


@user_bp.route('/remove/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    if user_id < 0:
        make_response(ErrorResponse(err_message='Invalid user ID'))
    status, response = UserHandler.delete_user(user_id)
    status_depend_response(status, response)


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





"""
API requests for users
"""


from flask import request, make_response, g, Response

from rpserver.db import engine
from ridersPlatform.db.models import User, UserRelation

from ..handlers.user import UserHandler
from .import user_bp


@user_bp.route('/add/', methods=['POST'])
def register_user():
    user_data = request.get_json()
    ###MAKE DATA VALIDATION
        make_response(Response(message='Not valid JSON fields', status=400, response={}))
    connection = engine.connect()
    if UserHandler.add_user(connection, user_data):
        make_response(Response(message='User has been added', status=200, response={}))
    else:
        make_response(Response(message='User has NOT been added', status=502, response={}))


@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    if user_id < 0:
        make_response(Response(message='Invalid user_id', status=400, response={}))
    response = UserHandler.get_user(connection, user_id)



@user_bp.route('/update/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    pass


@user_bp.route('/friends/', methods=['GET'])
def get_friends():
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


@user_bp.route('/remove/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    pass



"""
API requests for users
"""


from flask import request, make_response, jsonify, g

from ..valid_data_schema import PostUserSchema, PatchUserSchema
from . import user_bp


@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    if user_id < 0:
        make_response({'error': {'message': 'User id can not be less than 0'}}, 400)
    get_user_query = select([user_table]).where(user_table.c.user_id == user_id)
    connection = g.get('database')
    result = connection.execute(get_user_query).fetchall()
    make_response(result, 200)


@user_bp.route('/update/<int:user_id>', methods=['PATCH'])
def update_user(user_id):
    update_user_data = request.get_json()

    update_user_query = update([user_table]).where(user_table.c.user_id == user_id).values(update_user_data)
    connection = g.get('database')
    connection.execute(update_user_query)
    make_response({'message': 'User updated'}, 200)


@user_bp.route('/remove/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
 delete_user_query = delete([user_table]).where(user_table.c.user_id == user_id)
    connection = g.get('database')
    connection.execute(delete_user_query)
    make_response({'message': 'User has been deleted'}, 200)


@user_bp.route('/friends/<int:user_id>', methods=['GET'])
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





"""
API requests for users
"""


from flask import request, make_response, g


from ridersPlatform.api.blueprints.user import user_bp


@user_bp.route('/add/', methods=['POST'])
def register_user():
    pass


@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    pass


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



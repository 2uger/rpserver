"""
API requests for users
"""


from flask import request, g, make_response


from .import user_bp




@user_bp.route('/get/<int:user_id>', methods=['GET'])
def get_user(user_id):
    if user_id < 0:
        make_response(, 400)
    query = select([user_table]).where(user_table.c.user_id == user_id)
    with engine.connect() as connection:
        result = connection.execute(query).fetchall()
    make_response(result, 200)


@user_bp.route('/update/<int:user_id>', methods=['PATCH'])
def update_user(user_id):
    update_user_data = request.get_json()
    try:
        PatchUserSchema().load(update_user_data)
    except ValidationError as:
        #logging
        make_response('ValidationError', 400)
    query = update([user_table]).where(user_table.c.user_id == user_id).values(update_user_data)
    with engine.connect() as connection:
        response = connection.execute(query)
    make_response(response, 200)


@user_bp.route('/remove/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    if user_id < 0:
        make_response('Invalid USER ID', 400)
    query = delete([user_table]).where(user_table.c.user_id == user_id)
    with engine.connect() as connection:
        connection.execute(query)
    make_response(, 200)


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





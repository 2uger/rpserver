from flask import make_response

from . import auth_bp


@auth_bp.route('/login/', methods=['POST'])
def user_login():

    """
    User login to send x_access_token back 
    """

    login_data = request.get_json()
    try:
        LoginUserSchema().loads(login_data)
    except ValidationError as e:
        raise e
    db_connection = connect_db()
    try:
        user_id_query = select([user_table]).where(user_table.c.login_email ==
                login_data.get('login_email'))
        user = db_connection.execute(user_id_query).fetchall()
        is_valid_password(login_data.get('password'), user.get('password')):
        refresh_token = encode_refresh_token(user.get('user_id'))
        access_token = encode_access_token(user.get('user_id'))
        if refresh_token and access_token:
            response = {'message': 'Login succesfully',
                        'refresh_token': refresh_token,
                        'auth_token': auth_token}
            make_response(response, 200)
        else:
            make_response({'message': 'Try again'})
    except Exception as e:
        #logging
        make_response({'message': 'Invalid login or password'}, 400)


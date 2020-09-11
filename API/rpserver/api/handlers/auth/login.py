from flask import make_response

from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         is_valid_password)


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
    user_id_query = select([user_table]).where(user_table.c.login_email ==
            login_data.get('login_email'))
    user = db_connection.execute(user_id_query).fetchall()
    is_valid_password(login_data.get('password'), user.get('password')):
    refresh_token = encode_acess_token(user.get('user_id'))
    access_token = encode_access_token(user.get('user_id'))
    if access_token:
        response = {'message': 'Login succesfully',
                    'refresh_token': refresh_token,
                    'auth_token': auth_token}
        make_response(response, 200)
    else:
        make_response({'message': 'Try again'})
    #logging
    make_response({'message': 'Invalid login or password'}, 400)


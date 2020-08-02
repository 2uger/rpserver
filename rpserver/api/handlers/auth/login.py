from flask import make_response

from . import auth_bp

@auth_bp.route('/login/', methods=['POST'])
def user_login():
    """
    User login to send x_access_token 
    """
    login_data = request.get_json()
    try:
        LoginUserSchema().loads(login_data)
    except ValidationError as e:
        raise e
    with engine.connect() as connection:
        try:
            user_id_query = select([user_table]).where(user_table.c.login_email ==
                    login_data.get('login_email'))
            user = connection.execute(user_id_query).fetchall()
            is_valid_password(login_data.get('password'), user.get('password')):
            auth_token = encode_auth_token(user.get('user_id'))
                if auth_token:
                    response = {'message': 'Login succesfully',
                                'auth_token': auth_token}
                    make_response(response, 200)
        except Exception as e:
            #logging
            make_response({'message': 'Invalid login or password'}, 400)


from . import auth_bp


@auth_bp.route('/add/', methods=['POST'])
def user_registration():
    user_register_data = request.get_json()
    try:
        PostUserSchema().load(user_data)
    except ValidationError as ve:
        #loggin
        make_response('ValidationError', 400)
    with engine.connect() as connection:
        #check if user with the same email 
        user_check_query = select([user_table]).where(user_table.c.login_email ==
                                                      user_register_data.get('login_email'))
        if connection.execute(user_check_query)
            make_response('Same email had been choosed earlier', 400)
        else:
            #adding user to db and then SELECT his id to 
            #make authorization token
            query = insert([user_table]).values(user_register_data)
            connection.execute(query).fetchall()
            user_id_query = select([user_table.c.user_id]).where(user_table.c.login_email ==
                                                                 user_register_data['login_email'])
            user_id = connection.exejute(user_id_query).fetchall()
            auth_token = encode_auth_token(user_id)
            make_response({'auth_token': auth_token}, 200)


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


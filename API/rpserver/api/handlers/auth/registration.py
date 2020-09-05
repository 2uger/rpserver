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
            user_id = connection.execute(user_id_query).fetchall()
            refresh_token = encode_refresh_token(user.get('user_id'))
            access_token = encode_access_token(user.get('user_id'))
            make_response({'refresh_token': refresh_token,
                           'access_token': access_token},
                           200)



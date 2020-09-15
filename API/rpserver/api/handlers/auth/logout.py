from . import auth_bp


@auth_bp.route('/logout/', methods=['POST'])
def logout():

    """
    Adding refresh_token in blacklist with logout_time
    """

    refresh_token = request.headers.get('refresh_token', None)
    if refresh_token:
        user_id = decode_auth_token(auth_token)
        connection = db_connection()
        insert_values = {'user_id': user_id,
                         'refresh_token': refresh_token,
                         'logout_time': datetime.datetime()}
        blacklist_token_query = insert([blacklist_token_table]).values(insert_values)
        connection.execute(blacklist_token_query)
        make_response({'message': 'Logout'}, 200)
    else:
        make_response({'message': 'Provide valiable refresh_token'}, 400)

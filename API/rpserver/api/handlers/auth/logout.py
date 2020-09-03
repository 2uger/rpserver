from . import auth_bp


@auth_bp.route('/logout/', methods=['POST'])
def logout():
    """
    Adding token_auth in blacklist with logout_time
    """
    auth_token = request.headers.get('x_access_token', None)
    if auth_token:
        user_id = decode_auth_token(auth_token)
        connection = db_connection()
        insert_values = {'user_id': user_id,
                         'token': auth_token,
                         'logout_time': datetime.datetime()}
        blacklist_token_query = insert([blacklist_token_table]).values(insert_values)
        try:
            connection.execute(blacklist_token_query)
        except Exception as e:
            make_response('message': e)
        else:
            make_response('message': 'Logout')
    else:
        make_response({'message': 'Provide valiable auth_token'}, 400)

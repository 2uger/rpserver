from . import auth_bp

@auth_bp.route('/logout/', methods=['POST'])
def logout():
    auth_token = request.headers.get('x_access_token', None)
    if auth_token:
        user_id = decode_auth_token(auth_token)
        connection = db_connection()

        blacklist_token_query = insert([blacklist_token_table]).values({'user_id':
            user_id,
            'token': auth_token,
            'logout_time': datetime.datetime()})
        try:
            connection.execute(blacklist_token_query)
        except Exception as e:
            make_response('message': e)
    else:
        make_response({'message': 'Provide valiable auth_token'}, 400)

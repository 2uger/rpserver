"""
Flask middleware for 
-before_request
-after_request
-loggin
"""


from flask import make_response


def jwt_token_authorization():
    auth_token = request.headers.get('x-access-token')
    user_id = decode_auth_token(auth_token)
    if is_logout_token(user_id):
        make_response('Auth error', 400)
        return 'Auth error'
    else:
        return None

        

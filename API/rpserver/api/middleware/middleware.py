"""
Flask middleware for 
-before_request
-after_request
-loggin
"""


from flask import make_response


def jwt_token_authorization():
    if not access_token = request.headers.get('access-token'):
        make_response({'error': {'message': 'No access_token'}}, 401)
    try:
        user_id = decode_access_token(access_token)
    except jwt.ExpiredSignatureError:
        make_response({'error': {'message': 'Token expired'}}, 401)



        

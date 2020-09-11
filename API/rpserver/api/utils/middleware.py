"""
Flask middleware for 
-before_request
-after_request
-loggin
"""


from flask import make_response


def jwt_token_authorization():
    auth_token = request.headers.get('x-access-token')
    try:
        user_id = decode_auth_token(auth_token)
    except JWT_EXPIRITION_eRRoR:
        MAKE NEW X_ACCESS_TOKEN VIA REFRESH_TOKEN



        

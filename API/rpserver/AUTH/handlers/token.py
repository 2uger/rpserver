from flask import request, make_response
from jwt import IvalidTokenError

from . import auth_bp

from .auth_utils import (encode_access_token, 
                         encode_refresh_token, 
                         decode_access_token, 
                         decode_refresh_token)


@auth_bp.route('/token/access_token', methods=['GET'])
def gen_new_access_token():
    rider_id = decode_refresh_token(request.headers.get('refresh_token'))
    new_access_token = encode_access_token(rider_id)
    make_response({'access_token': new_access_token}, 200)


@auth_bp.route('/token/refresh_token', methods=['GET'])
def gen_new_refresh_token():
    rider_id = decode_refresh_token(request.headers.get('refresh_token'))
    new_refresh_token = encode_refresh_token(rider_id)
    make_response({'refresh_token': new_refresh_token}, 200)
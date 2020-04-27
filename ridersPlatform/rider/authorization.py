from flask import g
from flask_httpauth import HTTPBasicAuth, HTTPTokenAuth


from ridersPlatform.models import Rider
from ridersPlatform.responses import response_status

basic_auth = HTTPBasicAuth()
token_auth = HTTPTokenAuth()


@basic_auth.verify_password
def verify_password(login_email, password):
    rider = Rider.query.filter(Rider.login_email == login_email).first()
    if not rider:
        return None
    g.current_user = rider
    return rider.check_password(password)


@basic_auth.error_handler
def handle_auth_error():
    return response_status(message='Incorrect login or password', status_code=404)


@token_auth.verify_token
def verify_token(token):
    g.current_user = Rider.query.filter(Rider.token == token).first()
    return g.current_user is not None


@token_auth.error_handler
def handle_token_error():
    return response_status(message='Incorrect token', status_code=404)

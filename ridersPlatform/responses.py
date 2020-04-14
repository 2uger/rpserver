from flask import jsonify
from werkzeug.http import HTTP_STATUS_CODES


def response_json(message=None, status_code = 404):
    payload = {'error': HTTP_STATUS_CODES.get(status_code, 'Unknown error')}
    if message:
        payload['message'] = message
    response = jsonify(payload)
    response.status_code = status_code
    return response



"""
Response custom from BaseResponse Flask
"""


from flask import Response


class JsonResponse(Response):
    default_mimetype = 'application/json'
    default_status = 200

    def __init__(self, status_code, response):
        super(JsonResponse, self).__init__(status=status_code, response=response)


class ErrorResponse(Response):
    default_mimetype = 'application/json'
    default_status = 400

    def __init__(self, status_code, err_message):
        super(ErrorResponse, self).__init__(status=status_code,
                response=err_message)

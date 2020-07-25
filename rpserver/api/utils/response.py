"""
Response custom from BaseResponse Flask
"""


from flask import Response


class JSONResponse(Response):
    default_mimetype = 'application/json'
    default_status_code = 200
    default_status_message = 'OK'

    def __init__(self, status_message, status_code, response):
        super(JSONResponse, self).__init__(status=status_code, response=response)
        if status_message == '':
            self.response.update('status_message': self.default_status_message)

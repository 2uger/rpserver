from flask import Response

class JSONResponse(Response):
    default_mimetype = 'application/json'
    default_status = 200

    def __init__(self, message, status, response):
        super(JSONResponse, self).__init__(status=status, response=response)
        self.response.update('message': message)

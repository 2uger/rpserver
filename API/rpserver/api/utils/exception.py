from werkzeug.exceptions import BadRequest as BR
from sqlalchemy import InvalidArgumentError as IAE


exception_list = [BR, IAE]
handle_exception = []


def bad_request():
    make_response({'message': 'BadRequest'})


def invalid_sql_request():
    make_response({'message': 'InvalidSQLrequest'})

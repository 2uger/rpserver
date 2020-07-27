"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, current_app

from . import spot_bp

from rpserver.api.handlers import SpotHandler
from rpserver.api.utils import JSONResponse


@spot_bp.route('/add/', methods=['POST'])
def register_spot():
    status, response = SpotHandler.add_spot(request)
    status_depend_response(status)


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    status, response = SpotHandler.get_spot(spot_id)
    status_depend_response(status)


@spot_bp.route('/update/<int:spot_id>', methods=['PATCH'])
def update_spot(spot_id):
    status, response = SpotHandler.update_spot(request, spot_id)
    status_depend_response(status)


@spot_bp.route('/delete/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    status, response = SpotHandler.delete_spot(spot_id)
    status_depend_response(status)

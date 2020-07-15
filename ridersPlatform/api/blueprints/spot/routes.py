"""
API requests for spots

Accessible functions only for admin:
-register
-update
-remove
"""


from flask import request, make_response, current_app


from ridersPlatform.api.blueprints.spot import spot_bp


@spot_bp.route('/register/', methods=['POST'])
def register_spot():
    pass


@spot_bp.route('/get/<int:spot_id>', methods=['GET'])
def get_spot(spot_id):
    pass


@spot_bp.route('/update/<int:spot_id>', methods=['PUT'])
def update_spot(spot_id):
    pass


@spot_bp.route('/remove/<int:spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    pass




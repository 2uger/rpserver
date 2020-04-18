from flask import Blueprint, request, make_response, current_app

from ridersPlatform.models import Spot
from ridersPlatform.responses import response_status
from . import spot_bp


@spot_bp.route('/register', methods=['POST'])
def register_spot():
    spot_information = request.get_json()
    spot = Spot()
    Spot.add_to_db(spot.from_dict(spot_information))
    return response_status("Riders succefully register", 200)


@spot_bp.route('/get/<spot_id>', methods=['GET'])
def get_spot(spot_id):
    spot = Spot.query.filter(Spot.id == spot_id).first()
    if spot:
        return make_response(spot.to_dict(), 200)
    return response_status('No such spot', 400)


@spot_bp.route('/update/<spot_id>', methods=['PUT'])
def update_spot(spot_id):
    spot = Spot.query.filter(Spot.id == spot_id)
    spot_update = request.get_json()
    if spot and not Spot.query.filter(Spot.name == spot_update['name']).first():
        Spot.add_to_db(spot.to_dict())
        response_status('Spot succesfully updated', 200)
    return response_status('Spot with same name is exist', 400)


@spot_bp.route('/delete/<spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    spot = Spot.query.filter(Spot.id == spot_id)
    if spot:
        Spot.delet_from_db(spot)
        return response_status('Spot succefully added', 200)
    return response_status('No such spot to delete', 400)




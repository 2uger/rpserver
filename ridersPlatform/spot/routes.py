from flask import Blueprint, request, make_response, current_app

from ridersPlatform import db
from ridersPlatform.models import Spot
from ridersPlatform.responses import response_json
from . import spot_bp


@spot_bp.route('/register', methods=['POST'])
def register_spot():
    spot_information = request.get_json()
    spot_to_register = Spot()
    db.session.add(spot_to_register.from_dict(spot_information))
    db.session.commit()
    return make_response()


@spot_bp.route('/get/<spot_id>', methods=['GET'])
def get_spot(spot_id):
    spot = Spot.query.filter(Spot.id == spot_id).first()
    if spot:
        return make_response(spot.to_dict(), 200)
    return response_json('No such spot', 400)


@spot_bp.route('/update/<spot_id>', methods=['PUT'])
def update_spot(spot_id):
    spot = Spot.query.filter(Spot.id == spot_id)
    spot_update = request.get_json()
    if spot and not Spot.query.filter(Spot.name == spot_update['name']).first():
        spot.from_dict(spot_update)
        db.session.add(spot)
        db.session.commit()
    return response_json('Spot with same name is exist', 400)


@spot_bp.route('/delete/<spot_id>', methods=['DELETE'])
def delete_spot(spot_id):
    spot = Spot.query.filter(Spot.id == spot_id)
    if spot:
        db.session.delete(spot)
        db.session.commit()
        return response_json('Spot succefully added', 200)
    return response_json('No such spot to delete', 400)




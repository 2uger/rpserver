from flask import Blueprint, request, make_response, jsonify

from ridersPlatform import db
from ridersPlatform.models import Spot


spot = Blueprint('spot', __name__)

@spot.route('/register', methods=['POST'])
def register_spot():
    spot_information = request.get_json()
    spot_to_register = Spot(
            name=spot_information['name'],
            location=spot_information['location'],
            profile_image=spot_information['profile_image'],
            notes=spot_information['notes']
    )
    db.session.add(spot_to_register)
    db.session.commit()
    return make_response()

@spot.route('/get/<spot_id>')
def get_spot(spot_id):
    if spot_id:
       spot_information = Spot.query.filter_by(id=spot_id)
       return make_response(jsonify(spot_information), 200)
    else:
        spot_information = request.get_json()
        name = spot_information['name']
        spot_information = Spot.query.filter_by(name=name)
        return make_response(jsonify(spot_information), 200)



from flask import Blueprint, request, make_response, current_app

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


@spot.route('/get/<spot_id>', methods=['GET'])
def get_spot(spot_id):
    spot_information = Spot.query.filter(Spot.id == spot_id).first().to_dict()
    return make_response(spot_information, 200)



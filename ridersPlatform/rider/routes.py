from flask import Blueprint, request, make_response


from ridersPlatform.models import Rider
from ridersPlatform import db
from ridersPlatform.responses import response_json


rider_bp = Blueprint('rider_bp', __name__)


@rider_bp.route('/register', methods=['POST'])
def register_rider():
    rider_information = request.get_json() or {}
    if rider_information is None or len(rider_information) < 9:
        return response_json('Lack of information', 400)
    if Rider.query.filter_by(Rider.login_email == rider_information['login_email']).first():
        return response_json('User with the same login is exist', 406)
    rider = Rider()
    db.session.add(rider.from_dict(rider_information))
    db.session.commit()
    return make_response('Rider succefully added', 200)


@rider_bp.route('/get/<rider_id>', methods=['GET'])
def get_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    if rider:
        return make_response(rider.to_dict(), 200)
    return response_json('No such rider', 404)


@rider_bp.route('/change/<rider_id>', methods=['PUT'])
def update_rider(rider_id):
    rider = Rider.query.filter_by(Rider.id == rider_id).first()
    rider_update = request.get_json() or {}
    if Rider.query.filter_by(Rider.login_email == rider_update['login_email']).first():
        return response_json('Rider with the same login exist', 406)
    rider.from_dict(rider_update)
    db.session.add(rider)
    db.session.commit()
    return make_response('Rider succefully updated', 200)


@rider_bp.route('/delete/<rider_id>', methods=['DELETE'])
def delete_rider(rider_id):
    rider = Rider.query.filter_by(Rider.id == rider_id).first()
    if rider:
        db.session.delete(rider)
        db.session.commit()
    return response_json('No such rider founded', 404)

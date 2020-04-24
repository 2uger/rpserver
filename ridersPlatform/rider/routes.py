from flask import Blueprint, request, make_response


from ridersPlatform.models import Rider
from ridersPlatform import db
from ridersPlatform.responses import response_status
from . import rider_bp


@rider_bp.route('/register', methods=['POST'])
def register_rider():
    rider_information = request.get_json()
    if rider_information is None or len(rider_information) < 5:
        return response_status('Lack of information', 400)
    if Rider.query.filter(Rider.login_email == rider_information['login_email']).first():
        return response_status('Riders exist', 400)
    rider = Rider()
    rider.from_dict(rider_information)
    Rider.add_to_db(rider)
    return response_status('Rider succefully added', 200)


@rider_bp.route('/get/<rider_id>', methods=['GET'])
def get_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    if rider:
        return make_response(rider.to_dict(), 200)
    return response_status('No such rider', 404)


@rider_bp.route('/update/<rider_id>', methods=['PUT'])
def update_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    rider_update = request.get_json() or {}
    if not Rider.query.filter(Rider.login_email == rider_update['login_email']).first():
        return response_status('No such rider to update', 406)
    rider.from_dict(rider_update)
    Rider.add_to_db(rider)
    return response_status('Rider succefully updated', 200)


@rider_bp.route('/delete/<rider_id>', methods=['DELETE'])
def delete_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    if rider:
        Rider.delete_from_db(rider)
    return response_status('No such rider founded', 404)

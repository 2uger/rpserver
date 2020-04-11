from flask import Blueprint, request, jsonify, make_response
from flask_login import login_user

from ridersPlatform.models import Rider
from ridersPlatform import db

rider = Blueprint('rider', __name__)


@rider.route('/register', methods=['POST'])
def register_rider():
    rider_information = request.get_json()
    if rider_information is None or rider_information == {}:
        return make_response('No data to register', 400)
    rider_to_register = Rider(
        name=rider_information['name'],
        surname=rider_information['surname'],
        age=rider_information['age'],
        login_email=rider_information['login_email'],
        password=rider_information['password'],
        hometown=rider_information['hometown'],
        remember_me=rider_information['remember_me'],
        profile_image=rider_information['profile_image'],
    )
    db.session.add(rider_to_register)
    db.session.commit()
    return make_response('Rider succefully added', 200)


@rider.route('/get/<rider_id>', methods=['GET'])
def get_rider(rider_id):
    rider_obj = Rider.query.filter(Rider.id == rider_id).first()
    rider_information = rider_obj.to_dict()
    return make_response(rider_information, 200)


@rider.route('/login', methods=['GET', 'POST'])
def log_in_rider():
    rider_req = request.get_json()
    rider_db = Rider.query.filter_by(login_email=rider_req['login_email'])
    if rider_db:
        pass
        #make checking password

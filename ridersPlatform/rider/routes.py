from flask import Blueprint, request, jsonify, make_response
from flask_login import login_user

from ridersPlatform.models import Rider
from ridersPlatform import bcrypt, db


rider = Blueprint('rider', __name__)


@rider.route('/register', methods=['POST'])
def register_rider():
    rider_information = request.get_json()
    if rider_information is None:
        return make_response("")
    hashed_password = bcrypt.generate_password_hash(rider_information['password']).decode('utf-8')
    rider_to_register = Rider(
        name=rider_information['name'],
        surname=rider_information['surname'],
        age=rider_information['age'],
        login=rider_information['login'],
        password=hashed_password,
        hometown=rider_information['hometown'],
        profile_image=rider_information['profile_image'],
    )
    db.session.add(rider_to_register)
    db.session.commit()
    return jsonify('{}')


@rider.route('/get')
def get_rider():
    pass


@rider.route('/login', methods=['GET', 'POST'])
def log_in_rider():
    rider_req = request.get_json()
    rider_db = Rider.query.filter_by(login_email=rider_req['login_email'])
    if rider_db and bcrypt.check_password_hash(rider_db.password, rider_req['password']):
        pass
        #make checking password

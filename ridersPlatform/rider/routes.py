from flask import request, make_response, g


from ridersPlatform.models import Rider, Coordinates
from ridersPlatform import db
from ridersPlatform.responses import response_status
from ridersPlatform.rider import rider_bp
from ridersPlatform.authorization import basic_auth, token_auth


@rider_bp.route('/register', methods=['POST'])
def register_rider():
    rider_information = request.get_json()
    if rider_information is None or len(rider_information) < 5:
        return response_status('Lack of information', 400)
    if Rider.query.filter(Rider.login_email == rider_information['login_email']).first():
        return response_status('Riders exist', 400)
    rider = Rider()
    coordinates = Coordinates(rider=rider)
    rider.from_dict(rider_information)
    Rider.add_to_db(rider)
    return response_status('Rider succefully added', 200)


@rider_bp.route('/get/<rider_id>', methods=['GET'])
@token_auth.login_required
def get_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    if rider:
        return make_response(rider.to_dict(), 200)
    return response_status('No such rider', 404)


@rider_bp.route('/update/<rider_id>', methods=['PUT'])
@token_auth.login_required
def update_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    rider_update = request.get_json() or {}
    if not Rider.query.filter(Rider.login_email == rider_update['login_email']).first():
        return response_status('No such rider to update', 406)
    rider.from_dict(rider_update)
    Rider.add_to_db(rider)
    return response_status('Rider succefully updated', 200)


@rider_bp.route('/friends', methods=['GET'])
@token_auth.login_required
def get_friends():
    friends_id_dict = {'friends_id': g.current_user.friends_id}
    return make_response(friends_id_dict)


@rider_bp.route('/friendship_request/<rider_id>', methods=['PUT'])
@token_auth.login_required
def friendship_request(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    rider.friendship_request = rider.friendship_request + str(rider_id) + ','
    Rider.add_to_db(rider)
    return response_status('Friendship request send', 200)


@rider_bp.route('/make_friendship/<rider_id>', methods=['PUT'])
@token_auth.login_required
def make_friendship(rider_id):
    friendship_request = g.current_user.friendship_request.split(',')
    if rider_id in friendship_request:
        g.current_user.friends_id = g.current_user.friends_id + str(rider_id) + ','
        Rider.add_to_db(g.current_user)
        return response_status('Friendship is submit', 200)
    else:
        return response_status('Firstly make friendship request')


@rider_bp.route('/delete/<rider_id>', methods=['DELETE'])
@token_auth.login_required
def delete_rider(rider_id):
    rider = Rider.query.filter(Rider.id == rider_id).first()
    if rider:
        Rider.delete_from_db(rider)
    return response_status('No such rider founded', 404)


@rider_bp.route('/token', methods=['POST'])
@basic_auth.login_required
def get_token():
    token = g.current_user.get_token()
    db.session.commit()
    return make_response({'token': token}, 200)


@rider_bp.route('/token', methods=['DELETE'])
@token_auth.login_required
def delete_token():
    g.current_user.revoke_token()
    db.session.commit()
    return response_status('Token deleted', 200)



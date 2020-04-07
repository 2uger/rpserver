from flask import Blueprint
from flask import render_template
from flask import jsonify

from models import Rider

spots = Blueprint('spots', __name__, template_folder='templates')

@spots.route('/')
def index():
    riders = Rider.query.all()
    rider = dict()
    rider[str(riders[0])] = "oleg"
    return jsonify(rider)

@spots.route('/<pol>')
def find_by_id(pol):
 	rider = Rider.query.filter(Rider.id==pol).first()
 	return jsonify(str(rider))
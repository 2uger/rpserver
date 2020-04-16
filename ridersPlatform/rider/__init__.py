from flask import Blueprint


rider_bp = Blueprint('rider_bp', __name__)

from . import routes

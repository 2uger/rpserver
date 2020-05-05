from flask import Blueprint

coordinates_bp = Blueprint('coordinate_bp', __name__)

from . import routes
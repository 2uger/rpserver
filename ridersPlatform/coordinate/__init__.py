from flask import Blueprint

coordinate_bp = Blueprint('coordinate_bp', __name__)

from . import routes
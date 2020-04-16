from flask import Blueprint


spot_bp = Blueprint('spot', __name__)


from . import routes
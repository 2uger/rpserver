from flask import make_response, request

# TODO: rewrite imports
from  import auth_bp
from ..valid_data_schema import PostUserSchema
from .auth_utils import encode_access_token, encode_refresh_token


@auth_bp.route('/add/', methods=['POST'])
def user_registration():
    user_register_data = request.get_json()
    PostUserSchema().load(user_register_data)
    connection = g.get('database')
    
    # Check if there is user with the same email
    with connection.cursor() as cur:
        rider_check_query = "SELECT email FROM rider WHERE email = %s";
        cur.execute(rider_check_query)
        result = connection.fetchone()
        if result:
            make_response({"msg": "Wrong email", 404)

        rider_insert_query = "INSERT INTO rider(nickname, email, hometown, registration_date) \ 
                              VALUES(%s, %s, %s, %s)"
        cur.execute(rider_insert_query, (rider_registration_data["nickname"],
                                         rider_registration_data["email"],
                                         rider_registration_data["hometown"]
                                         datetime.today()))

    make_response({"msg": "Registration completed", 200)



"""
Objects of database

Common methods:

    def is_valid(self, input_data)->bool:
        """
        Return {failed_field: error_description} if data is valid
        else return True
        """
"""


class User(object):
    def __init__(self, name, surname, login_email, age, bio, 
                 profile_image_url, hometown, registration_date, event):
        self.name = name,
        self.surname = surname,
        self.login_email = login_email,
        self.age = age,
        self.bio = bio,
        self.profile_image_url = profile_image_url,
        self.hometown = hometown,
        self.registration_date = registration_date,
        self.event = event

    def __repr__(self):
        pass
    
    def is_valid(self, input_data)->bool:
        """
        Return {failed_field: error_description} if data is valid
        else return True
        """
        pass

class UserRelation(object):
    def __init__(self, user_id, relation_id, relation_type):
        self.user_id = user_id,
        self.relation_id = relation_id,
        self.relation_type = relation_type

    def __repr__(self):
        pass  

    def is_valid(self, input_data)->bool:
        pass

class Spot(object):
    def __init__(self, name, location, notes, profile_image_url):
        self.name = name,
        self.location = location,
        self.notes = notes,
        self.profile_image_url = profile_image_url

    def __repr__(self):
        pass

    def is_valid(self, input_data)->bool:
        pass

class Coordinates(object):
    def __init__(self, longitude, latitude):
        self.longitude = longitude,
        self.latitude = latitude

    def __repr__(self):
        pass

    def is_valid(self, input_data)->bool:
        pass

class Event(object):
    def __init__(self, description, datetime):
        self.description = description,
        self.datetime = datetime

    def __repr__(self):
        pass

    def is_valid(self, input_data)->bool:
        pass

"""
BaseHandler
"""


from sqlalchemy.sql import select, insert, update, delete


from rpserver.db import engine


class Handler:
    def __init__(self):
        pass

    @staticmethod
    def post(request, ValidationSchema, connection,):
        object_data = request.get_json()
        ValidationSchema().load(object_data)
        query = insert([db_table]).values(insert_data)
        result = connection.execute(query).fetchall()
        return result 

    @staticmethod
    def get(connection, db_table, object_id):
        if object_id < 0:
            return 'Invalid ID'
        query = select([db_table]).where(db_table.c.id == object_id)
        connection.execute(query)

    @staticmethod
    def patch(request, ValidationSchema, connection, db_table, object_id):
        patch_update_data = request.get_json()
        ValidationSchema().load(patch_data)
        query = update([db_table]).where(db_table.c.id == object_id).values(update_data)
        connection.execute(query)

    @staticmethod
    def delete(connection, db_table, object_id):
        query = delete([db_table]).where(db_table.c.id==object_id)
        result = connection.execute(query)
        return result




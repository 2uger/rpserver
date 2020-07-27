"""
Handler for making post, get, patch, delete requests
"""


from sqlalchemy.sql import select, insert, update, delete


class Handler:
    def __init__(self):
        pass

    @staticmethod
    def post(request, validation_schema, connection, db_table):
        object_data = request.get_json()
        validation_schema().load(object_data)
        query = insert([db_table]).values(object_data)
        result = connection.execute(query).fetchall()
        return result 

    @staticmethod
    def get(connection, db_table, object_id):
        if object_id < 0:
            return 'Invalid ID'
        query = select([db_table]).where(db_table.c.id == object_id)
        connection.execute(query)

    @staticmethod
    def patch(request, validation_schema, connection, db_table, object_id):
        patch_update_data = request.get_json()
        validation_schema().load(patch_update_data)
        query = update([db_table]).where(db_table.c.id == object_id).values(patch_update_data)
        connection.execute(query)

    @staticmethod
    def delete(connection, db_table, object_id):
        query = delete([db_table]).where(db_table.c.id == object_id)
        result = connection.execute(query)
        return result




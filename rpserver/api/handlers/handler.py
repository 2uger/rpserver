"""
BaseHandler
"""


from sqlalchemy.sql import select, insert, update, delete


from rpserver.db import engine


class BaseHandler:
    def __init__(self):
        pass

    @staticmethod
    def post(db_table, insert_data):
        query = insert([db_table]).values(insert_data)
        return execute_query(query)

    @staticmethod
    def get(db_table, object_id):
        query = select([db_table]).where(db_table.c.id == object_id)
        return execute_query(query)

    @staticmethod
    def patch(self, db_table, object_id, update_data):
        query = update([db_table]).where(db_table.c.id == object_id).values(update_data)
        return execute_query(query)

    @staticmethod
    def delete(self, db_table, object_id):
        query = delete([db_table]).where(db_table.c.id==object_id)
        return execute_query(query)

    @staticmethod
    def execute_query(query):
        try:
            with engine.connect() as connection:
                result = connection.execute(query).fetchall()
        except Exception as e:
            #make loggin
            return ('', {})
        else:
            return ('', result)



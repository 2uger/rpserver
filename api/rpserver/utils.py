def db_select(table_name, db_connection, obj_id=None, many=False):
    """Select obj from db."""
    if not many:
        get_obj_query = f"""SELECT * FROM {table_name} WHERE id = %s"""
        with db_connection.cursor() as cur:
            cur.execute(get_obj_query, (obj_id,))
            obj = cur.fetchone()
        return obj
    else:
        get_objs_query = f"""SELECT * FROM {table_name}"""
        with db_connection.cursor() as cur:
            cur.execute(get_objs_query)
            objs = cur.fetchall()
        return objs


def db_update(obj_info, table_name, obj_id, db_connection):
    """Update table in db."""
    fields_keys = []
    fields_values = []
    for k, v in obj_info.items():
        fields_keys.append(k + '=%s')
        fields_values.append(v)

    update_obj_query = f"""UPDATE {table_name} SET {', '.join(fields_keys)} WHERE id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(update_obj_query, fields_values + [obj_id]) 


def db_insert(obj_info, table_name, db_connection):
    fields_keys = []
    fields_values = []
    for k, v in obj_info.items():
        fields_keys.append(k)
        fields_values.append(v)

    insert_obj_query = f"""INSERT INTO {table_name}({', '.join(fields_keys)}) VALUES({', '.join(['%s'] * len(fields_keys))})"""
    with db_connection.cursor() as cur:
        cur.execute(insert_obj_query, fields_values)


def db_delete(table_name, obj_id, db_connection):
    """Delete obj from db."""
    delete_obj_query = f"""DELETE FROM {table_name} WHERE id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(delete_obj_query, (obj_id,))


def serialize(db_obj):
    resp = {}
    for field, value in db_obj.items():
        resp[field] = value
    return resp


def serialize_many(db_objs):
    resp = []
    for obj in db_objs:
        resp.append(serialize(obj))
    return resp

def spots_by_area(db_connection, area_coordinates):
    """Return spots in certain area."""
    top_left_point = area_coordinates[0]
    bottom_right_point = area_coordinates[1]
    
    get_spot_by_area_query = """SELECT id, title, coordinates, notes FROM spot WHERE coordinates[0]>%s
                                                                           AND coordinates[0]<%s
                                                                           AND coordinates[1]>%s
                                                                           AND coordinates[1]<%s"""
    with db_connection.cursor() as cur:
        cur.execute(get_spot_by_area_query, (top_left_point[0], bottom_right_point[0],
                                             top_left_point[1], bottom_right_point[1]))
        spots_by_area = cur.fetchall()
    return spots_by_area

def events_by_spot(spot_id, db_connection):
    """Return all events by spot id."""
    events_by_spot = """SELECT * FROM ocassion WHERE spot_id=%s"""
    with db_connection.cursor() as cur:
        cur.execute(events_by_spot, (spot_id,))
        events = cur.fetchall()
    return events


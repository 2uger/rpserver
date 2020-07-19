"""
API requests for event on map
"""


from ridersPlatform.api.blueprints.event import event_bp


@event_bp.route('/register/', methods=['POST'])
def register_event():
    pass


@event_bp.route('/get/<int:event_id>', methods=['GET'])
def get_event(event_id):
    pass


@event_bp.route('/update/<int:event_id>', methods=['PUT'])
def update_event(event_id):
    pass


@event_bp.route('/remove/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    pass

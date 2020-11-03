def deserialize_to_dict(result):
    resp = {}
    if type(result) is list:
        for x in result:
            resp.append(dict(x))
    else:
        resp.append(dict(result))
    return resp

import collections, json, urllib.request, urllib.error

base = 'http://localhost:8080'
def request(path, data=None):
    req = urllib.request.Request(base + path, data=None if data is None else json.dumps(data).encode(), headers={'Content-Type': 'application/json'})
    try:
        with urllib.request.urlopen(req, timeout=30) as response:
            return response.status, json.load(response)
    except urllib.error.HTTPError as error:
        return error.code, json.load(error)

assert request('/api/health') == (200, {'status':'ok'})
code, board = request('/api/solve', {'letters':'cat'})
assert code == 200 and board['status'] == 'solved', board
rows = board['rows']
assert collections.Counter(''.join(rows).replace(' ', '')) == collections.Counter('cat'), rows
assert request('/api/solve', {'letters':'c4t'})[0] == 400
assert request('/api/solve', {'letters':''})[0] == 400
assert request('/api/solve', {'letters':'a'*26})[0] == 400
code, result = request('/api/solve', {'letters':'zzzz'})
assert code == 200 and result['status'] == 'not_found', result
print('API smoke checks passed')

class http:
    import requests as _requests

    @classmethod
    def get(cls, endpoint, headers):
        return cls._requests.get(endpoint, headers=headers)

    @classmethod
    def post(cls, endpoint, headers, json):
        return cls._requests.post(endpoint, headers=headers, json=json)

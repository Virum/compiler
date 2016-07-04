var http = new (function (http, https, url) {
  this.post = function post(endpoint, headers, json) {
    var data = JSON.stringify(json);
    var options = url.parse(endpoint);
    var protocol = {"http:": http, "https:": https}[options.protocol];
    options.method = "POST";
    options.headers = headers;
    var promise = new (function Promise() {
      this.then = function (callback) {
        var request = protocol.request(options, callback);
        request.write(data);
        request.end();
      }
    })();
    return promise;
  };
})(http, https, url);

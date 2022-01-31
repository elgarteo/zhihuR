var http = require('http'), url = require('url');
var secret = require('./secret');

http.createServer(function(request, response) {
    response.writeHead(200, {'Content-Type': 'text/plain'});
    var urlParts = url.parse(request.url, true);
    var string = secret.Q(urlParts.pathname.substring(1)); // remove preceding slash & sign
    response.end(string);
}).listen(3000);

console.log('Server running at http://127.0.0.1:3000/');

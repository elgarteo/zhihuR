var http = require("http"), url = require('url');
var encrypt = require('./encrypt');

http.createServer(function (request, response) {
    response.writeHead(200, {'Content-Type': 'text/plain'});
    var urlParts = url.parse(request.url, true),
        string = urlParts.pathname.substring(1); // remove preceding slash
    string = encrypt.Q(string);
    response.end(string);
}).listen(3000);

console.log('Server running at http://127.0.0.1:3000/');

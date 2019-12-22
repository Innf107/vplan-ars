"use strict";
function parseCookies(request) {
    var list = {}, rc = request.headers.cookie;
    rc && rc.split(';').forEach(function (cookie) {
        var parts = cookie.split('=');
        list[parts.shift().trim()] = decodeURI(parts.join('='));
    });
    return list;
}
var userLog = function (onUserReq) { return function (req, res, next) {
    if (req.method === "GET") {
        var cookies = parseCookies(req);
        onUserReq(req.path, cookies['accessTimestamp'] ? new Date(Number.parseInt(cookies['accessTimestamp'])) : null);
        res.cookie('accessTimestamp', Date.now(), { maxAge: 99999999 });
    }
    next();
}; };
module.exports = userLog;

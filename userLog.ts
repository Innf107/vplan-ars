import express = require('express')
import cookieParser = require('cookie-parser')

function parseCookies (request: express.Request) {
    var list = {},
        rc = request.headers.cookie;

    rc && rc.split(';').forEach(function( cookie ) {
        var parts = cookie.split('=');
        list[parts.shift().trim()] = decodeURI(parts.join('='));
    });

    return list;
}


const userLog = (onUserReq: (reqPath: string, lastVisit: Date) => void) => (req: express.Request, res: express.Response, next: express.NextFunction) => {
    if(req.method === "GET"){
        const cookies = parseCookies(req);
        onUserReq(req.path, cookies['accessTimestamp'] ? new Date(Number.parseInt(cookies['accessTimestamp'])) : null)
        res.cookie('accessTimestamp', Date.now(), {maxAge:999999999999})
    }
    next()
}


export = userLog;
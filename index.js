"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = y[op[0] & 2 ? "return" : op[0] ? "throw" : "next"]) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [0, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var _this = this;
exports.__esModule = true;
var express = require("express");
var xcss = require("prophet-xcss");
var iso88592 = require("iso-8859-2");
var fs = require("fs-jetpack");
var axios_1 = require("axios");
var http = require("http");
var https = require("https");
var sanitize = require("sanitize-filename");
var library_1 = require("./library");
var userLog = require("./userLog");
var app = express();
var HTTPPORT = 5000;
var HTTPSPORT = 5001;
var UPDATERATE = 590000;
var data;
var parsePlan = function (n) { return __awaiter(_this, void 0, void 0, function () {
    var url, res, data, dateRgx, dateStr, tableRgx, tableStr, klassen, motdAffectedRowRgx, motdAffected, motdContentRowRgxG, motdContentRowRgx, motdContentRows, motdContent, motd, refreshRgx, _a, _b, _c, _d, _e, _f;
    return __generator(this, function (_g) {
        switch (_g.label) {
            case 0:
                url = "https://vplan.ars-hochtaunus.de/subst_" + n.toString().padStart(3, '0') + ".htm";
                return [4 /*yield*/, axios_1["default"].get(url, {
                        auth: {
                            username: "vplan", password: "ars2013"
                        },
                        responseType: 'arraybuffer',
                        responseEncoding: 'binary'
                    })["catch"](function (_) { return null; })];
            case 1:
                res = _g.sent();
                if (res === null)
                    return [2 /*return*/, library_1.log("resIsNull: ", { vplan: {} })];
                data = iso88592.decode(res.data.toString('binary'));
                dateRgx = /<div\s+class="mon_title">(.+?)( \(.+?)?<\/div>/;
                dateStr = data.match(dateRgx)[1];
                tableRgx = /<table\s+class="mon_list"\s*>[^]+?<\/table>/;
                tableStr = data.match(tableRgx)[0];
                klassen = library_1.mapToObj(matchKlassen(tableStr), (function (match) {
                    return [match[2], {
                            name: match[2],
                            hours: matchHours(match[3])
                        }];
                }));
                if (!klassen)
                    return [2 /*return*/, library_1.log("!klassen", { vplan: {} })];
                motdAffectedRowRgx = /<td class="info" align="left">([^B][^]+?)<\/td>/;
                motdAffected = data.match(motdAffectedRowRgx)[1].split(',').map(function (x) { return x.trim(); });
                motdContentRowRgxG = /<td class='info' colspan="2">[^]+?<\/td>/g;
                motdContentRowRgx = /<td class='info' colspan="2">([^]+?)<\/td>/;
                motdContentRows = data.match(motdContentRowRgxG);
                motdContent = motdContentRows === null ? [] : motdContentRows.map(function (x) { return x.match(motdContentRowRgx)[1]; });
                motd = {
                    affected: motdAffected,
                    content: motdContent
                };
                refreshRgx = /<meta http-equiv="refresh" content="12; URL=subst_001.htm">/;
                if (refreshRgx.test(data))
                    return [2 /*return*/, {
                            vplan: (_e = {}, _e[dateStr] = {
                                day: dateStr,
                                motd: motd,
                                klassen: klassen
                            }, _e)
                        }];
                _a = {};
                _b = library_1.sortKeysBy;
                _c = mergeData;
                _d = [(_f = {}, _f[dateStr] = {
                        day: dateStr,
                        motd: motd,
                        klassen: klassen
                    }, _f)];
                return [4 /*yield*/, parsePlan(n + 1)];
            case 2: return [2 /*return*/, (_a.vplan = _b.apply(void 0, [_c.apply(void 0, _d.concat([((_g.sent()).vplan)])), compareDates]),
                    _a)];
        }
    });
}); };
var mergeData = function (a, b) { return library_1.mergeObjWith(a, b, function (x, y) {
    return {
        day: x.day,
        motd: y.motd,
        klassen: library_1.mergeObjWith(x.klassen, y.klassen, function (x, y) { return { name: x.name, hours: library_1.mergeObjWith(x.hours, y.hours, function (x, y) { return y; }) }; })
    };
}); };
var compareDates = function (x, y) {
    var xDate = Date.parse(x.match(/[0-9.]+/)[0]);
    var yDate = Date.parse(y.match(/[0-9.]+/)[0]);
    return xDate - yDate;
};
/*
const mergeKlassen = (a: UntisKlasse[], b: UntisKlasse[]) : UntisKlasse[] => mergeByWith(a, b,
    (x, y) => x.name === y.name,
    (x, y) => {return {
        name:x.name,
        hours: x.hours.concat(y.hours)
    }})
*/
var matchKlassen = function (tableStr) {
    var klasseRgx1 = /(<tr\s+class=["']list .+?["']>\s*<td class=["']list inline_header["'].*?>[^]+?<\/td>\s*<\/tr>[^]+?)(?:(?:<tr\s+class=['"]list .+?['"]><td class=['"]list inline_header['"].*?>)|(?:<\/table>))/;
    var klasseRgx = /(<tr\s+class=["']list .+?["']>\s*<td class=["']list inline_header["'].*?>([^]+?)<\/td>\s*<\/tr>([^]+))/;
    var match = tableStr.match(klasseRgx1);
    if (match == null)
        return [];
    else {
        var remaining = tableStr.replace(match[1], '');
        return [match[1].match(klasseRgx)].concat(matchKlassen(remaining));
    }
};
var matchHours = function (childrenStr) {
    //                                                                             Stunde                                     Vertreter                                                Fach                                           Raum                                      Vertretungs-Text
    //                                                                             1 - 2                                         DOB                                                   MAT                                            E10                                          Stattstunde
    var hourRgx = /<tr class='list[^]*?'>\s*<td class="list" align="center">([^]+?)<\/td>\s*<td class="list" align="center">([^]+?)<\/td>\s*<td class="list"(?: align="center")?>([^]+?)<\/td>\s*<td class="list"(?: align="center")?>([^]+?)<\/td>\s*<td class="list"(?: align="center")>([^]+?)<\/td>\s*<\/tr>/;
    var results = library_1.mapToObj(library_1.matchAll(hourRgx, childrenStr), (function (match) { return [match[1], {
            stunde: match[1],
            vertreter: match[2],
            fach: match[3],
            raum: match[4],
            vtext: match[5]
        }]; }));
    return results;
};
//API
app.use(xcss(["./public"]));
var totalUsers = 0;
app.use(userLog(function (reqPath, lastVisit) {
    if (lastVisit === null)
        totalUsers++;
}));
app.get('/pro', library_1.staticFile('public/index.html'));
app.get('/beta', library_1.staticFile('public/beta.html'));
app.get('/main.css', library_1.staticFile('public/main.css'));
app.get('/select.css', library_1.staticFile('select.css'));
app.get('/teacher.css', library_1.staticFile('teacher.css'));
app.get('/', library_1.staticFile('public/select.html'));
app.get('/teacher', library_1.staticFile('public/teacher.html'));
app.get('/robots.txt', library_1.staticFile('robots.txt'));
app.get('/index.js', library_1.staticFile('public/index.js'));
app.get('/teacher.js', library_1.staticFile('public/teacher.js'));
app.get('/personal', library_1.staticFile('public/personal.html'));
app.get('/personal.js', library_1.staticFile('public/personal.js'));
app.get('/sw.js', library_1.staticFile('public/sw.js'));
app.get('/manifest.webmanifest', library_1.staticFile('manifest.webmanifest'));
app.get('/logoMain.png', library_1.staticFile('logoMain.png'));
app.get('/usersTotal', function (req, res) { return res.send(totalUsers.toString()); });
app.get('/json/kuerzel', function (req, res) {
    var json = JSON.parse(fs.read('kuerzel.json'));
    res.send(Object.keys(json).map(function (k) { return { key: k, value: json[k] }; }));
});
app.get('/json', function (req, res) {
    res.json(data || "Parsing...");
});
app.put('/feedback', function (req, res) {
    var fileName = 'temp.txt';
    var filePath = "../data/Feedback";
    var file = fs.createWriteStream(filePath + "/" + fileName);
    file.on('open', function (_) {
        req.on('data', function (x) { return file.write(x); });
        req.on('end', function () {
            file.end();
            var content = fs.read(filePath + "/" + fileName);
            fs.renameAsync(filePath + "/" + fileName, sanitize(content.replace(/\//g, '\\')));
            res.sendStatus(200);
        });
    });
});
app.get('/*', function (req, res) {
    res.status(404).sendFile(__dirname + '/public/404.html');
});
try {
    var privateKey = fs.read('/etc/letsencrypt/live/vplan-ars.spdns.de/privkey.pem', 'utf8') || library_1.logOnly("private key not found for vplan-ars.spdns.de. trying localhost...", fs.read('localhost-key.pem', 'utf8')) || library_1.logOnly('private key for localhost does not exist either!', undefined);
    var certificate = fs.read('/etc/letsencrypt/live/vplan-ars.spdns.de/cert.pem', 'utf8') || library_1.logOnly("certificate not found for vplan-ars.spdns.de. trying localhost...", fs.read('localhost.pem', 'utf8')) || library_1.logOnly('certificate for localhost does not exist either!', undefined);
    var ca = fs.read('/etc/letsencrypt/live/vplan-ars.spdns.de/chain.pem', 'utf8');
    var credentials = {
        key: privateKey,
        cert: certificate
    };
    if (!privateKey || !certificate)
        throw new Error("credentials do not exist!");
    var httpsServer = https.createServer(credentials, app);
    httpsServer.listen(HTTPSPORT, function () { return console.log("HTTPS Server listening on port " + HTTPSPORT); });
    var httpserver = express();
    httpserver.get('*', function (req, res) { return res.redirect("https://" + req.headers.host + req.url); });
    httpserver.listen(HTTPPORT, function () { return console.log("HTTP Server listening on Port " + HTTPPORT); });
}
catch (_a) {
    var httpserver = http.createServer(app);
    httpserver.listen(HTTPPORT, function () { return console.log("HTTP Server listening on Port " + HTTPPORT); });
    console.log('cannot create HTTPS Server');
}
var update = function () { return __awaiter(_this, void 0, void 0, function () {
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, parsePlan(1)];
            case 1:
                data = _a.sent();
                console.log("updated vplan at " + new Date());
                fs.write('../data/totalUsers', totalUsers.toString());
                return [2 /*return*/];
        }
    });
}); };
fs.readAsync('../data/totalUsers').then(function (s) { return totalUsers = Number.parseInt(s); });
setInterval(update, UPDATERATE);
update();

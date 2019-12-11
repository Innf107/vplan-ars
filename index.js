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
var lite = require('./lite');
var app = express();
var PORT = 5000;
var UPDATERATE = 600000;
var data;
var parsePlan = function (n) { return __awaiter(_this, void 0, void 0, function () {
    var url, res, data, refreshRgx, dateRgx, dateStr, tableRgx, tableStr, klassen, motdAffectedRowRgx, motdAffected, motdContentRowRgxG, motdContentRowRgx, motdContentRows, motdContent, motd, _a, _b, _c;
    return __generator(this, function (_d) {
        switch (_d.label) {
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
                res = _d.sent();
                if (res === null)
                    return [2 /*return*/, { vplan: [] }];
                data = iso88592.decode(res.data.toString('binary'));
                refreshRgx = /<meta http-equiv="refresh" content="12; URL=subst_001.htm">/;
                dateRgx = /<div\s+class="mon_title">(.+?)( \(.+?)?<\/div>/;
                dateStr = data.match(dateRgx)[1];
                tableRgx = /<table\s+class="mon_list"\s*>[^]+?<\/table>/;
                tableStr = data.match(tableRgx)[0];
                klassen = matchKlassen(tableStr).map(function (match) {
                    return {
                        name: match[2],
                        hours: matchHours(match[3])
                    };
                });
                if (!klassen)
                    return [2 /*return*/, { vplan: [] }];
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
                if (refreshRgx.test(data))
                    return [2 /*return*/, {
                            vplan: [{
                                    day: dateStr,
                                    motd: motd,
                                    klassen: klassen
                                }]
                        }];
                _a = {};
                _b = mergeData;
                _c = [[{
                            day: dateStr,
                            motd: motd,
                            klassen: klassen
                        }]];
                return [4 /*yield*/, parsePlan(n + 1)];
            case 2: return [2 /*return*/, (_a.vplan = _b.apply(void 0, _c.concat([(_d.sent()).vplan])),
                    _a)];
        }
    });
}); };
var mergeData = function (a, b) { return mergeByWith(a, b, function (x, y) { return x.day === y.day; }, function (x, y) {
    return {
        day: x.day,
        motd: y.motd,
        klassen: x.klassen.concat(y.klassen)
    };
}).sort(compareDates); };
var compareDates = function (x, y) {
    var xDate = Date.parse(x.day.match(/[0-9.]+/)[0]);
    var yDate = Date.parse(y.day.match(/[0-9.]+/)[0]);
    return xDate - yDate;
};
var mergeKlassen = function (a, b) { return mergeByWith(a, b, function (x, y) { return x.name === y.name; }, function (x, y) {
    return {
        name: x.name,
        hours: x.hours.concat(y.hours)
    };
}); };
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
    //                                                                         Stunde                                     Vertreter                                                Fach                                           Raum                                      Vertretungs-Text
    //                                                                             1 - 2                                         DOB                                                   MAT                                            E10                                          Stattstunde
    var hourRgx = /<tr class='list[^]*?'>\s*<td class="list" align="center">([^]+?)<\/td><td class="list" align="center">([^]+?)<\/td>\s*<td class="list"(?: align="center")?>([^]+?)<\/td>\s*<td class="list" align="center">([^]+?)<\/td>\s*<td class="list" align="center">([^]+?)<\/td>\s*<\/tr>/;
    var results = matchAll(hourRgx, childrenStr).map(function (match) {
        return {
            stunde: match[1],
            vertreter: match[2],
            fach: match[3],
            raum: match[4],
            vtext: match[5]
        };
    });
    return results;
};
//API
app.use(xcss(["./public"]));
app.use('/lite', lite);
app.get('/pro', function (req, res) { return res.sendFile(__dirname + '/public/index.html'); });
app.get('/main.css', function (req, res) { return res.sendFile(__dirname + '/public/main.css'); });
app.get('/select.css', function (req, res) { return res.sendFile(__dirname + '/select.css'); });
app.get('/', function (req, res) { return res.sendFile(__dirname + '/public/select.html'); });
app.get('/json', function (req, res) {
    res.json(data || "Parsing...");
});
app.put('/feedback', function (req, res) {
    var date = new Date();
    var fileName = 'temp.txt';
    var filePath = "../data/Feedback";
    var file = fs.createWriteStream(filePath + "/" + fileName);
    file.on('open', function (_) {
        req.on('data', function (x) { return file.write(x); });
        req.on('end', function () { return __awaiter(_this, void 0, void 0, function () {
            var content;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        file.end();
                        content = fs.read(filePath + "/" + fileName);
                        return [4 /*yield*/, fs.renameAsync(filePath + "/" + fileName, content.replace(/\//g, '\\'))];
                    case 1:
                        _a.sent();
                        res.sendStatus(200);
                        return [2 /*return*/];
                }
            });
        }); });
    });
});
app.get('/*', function (req, res) {
    res.sendFile(__dirname + '/public/404.html');
});
app.listen(PORT, function () { return console.log("listening on Port " + PORT); });
var update = function () { return __awaiter(_this, void 0, void 0, function () {
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, parsePlan(1)];
            case 1:
                data = _a.sent();
                console.log("updated at " + new Date());
                return [2 /*return*/];
        }
    });
}); };
setInterval(update, UPDATERATE);
update();
//Helpers
var matchAll = function (rgx, str) {
    var match = str.match(rgx);
    if (match === null)
        return [];
    else {
        var remaining = str.replace(match[0], '');
        return [match].concat(matchAll(rgx, remaining));
    }
};
var wait = function (time) { return new Promise(function (resolve) { return setTimeout(function () { return resolve(time); }, time); }); };
function mergeByWith(a, b, match, resolve) {
    if (a[0] === undefined)
        return b;
    if (b[0] === undefined)
        return a;
    var x = a[0], xs = a.slice(1);
    var y = b[0], ys = b.slice(1);
    if (match(x, y))
        return [resolve(x, y)].concat(mergeByWith(xs, ys, match, resolve));
    else
        return [y, x].concat(mergeByWith(xs, ys, match, resolve));
}

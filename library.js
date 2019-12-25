"use strict";
var __assign = (this && this.__assign) || Object.assign || function(t) {
    for (var s, i = 1, n = arguments.length; i < n; i++) {
        s = arguments[i];
        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
            t[p] = s[p];
    }
    return t;
};
exports.__esModule = true;
var path = require("path");
function split(a, f) {
    if (a.length === 0)
        return [[], []];
    var x = a[0], xs = a.slice(1);
    var _a = split(xs, f), passed = _a[0], failed = _a[1];
    if (f(x))
        return [[x].concat(passed), failed];
    else
        return [passed, [x].concat(failed)];
}
exports.split = split;
function mergeObjWith(a, b, f) {
    var entriesA = Object.entries(a);
    var entriesB = Object.entries(b);
    var mergedEntries = mergeByWith(entriesA, entriesB, function (x, y) { return x[0] === y[0]; }, function (_a, _b) {
        var xk = _a[0], xv = _a[1];
        var yk = _b[0], yv = _b[1];
        return [xk, f(xv, yv)];
    });
    return objFromEntries(mergedEntries);
}
exports.mergeObjWith = mergeObjWith;
function mergeByWith(a, b, match, resolve) {
    if (a[0] === undefined)
        return b;
    if (b[0] === undefined)
        return a;
    var x = a[0], xs = a.slice(1);
    var _a = split(b, function (y) { return match(x, y); }), matched = _a[0], failed = _a[1];
    if (matched.length > 0) {
        return [[x].concat(matched).reduce(function (x, y) { return resolve(x, y); })].concat(mergeByWith(xs, failed, match, resolve));
    }
    else
        return [x].concat(mergeByWith(xs, b, match, resolve));
}
exports.mergeByWith = mergeByWith;
exports.matchAll = function (rgx, str) {
    var match = str.match(rgx);
    if (match === null)
        return [];
    else {
        var remaining = str.replace(match[0], '');
        return [match].concat(exports.matchAll(rgx, remaining));
    }
};
exports.wait = function (time) { return new Promise(function (resolve) { return setTimeout(function () { return resolve(time); }, time); }); };
function log(msg, x) {
    console.log(msg + " " + x);
    return x;
}
exports.log = log;
function logOnly(msg, x) {
    console.log(msg);
    return x;
}
exports.logOnly = logOnly;
exports.staticFile = function (_path) { return function (req, res) { return res.sendFile(path.resolve(_path)); }; };
function mapToObj(list, f) {
    if (list.length === 0)
        return {};
    var x = list[0], xs = list.slice(1);
    var _a = f(x), k = _a[0], v = _a[1];
    return __assign((_b = {}, _b[k] = v, _b), mapToObj(xs, f));
    var _b;
}
exports.mapToObj = mapToObj;
function objFromEntries(entries) {
    if (entries.length === 0)
        return {};
    var _a = entries[0], k = _a[0], v = _a[1], xs = entries.slice(1);
    return __assign((_b = {}, _b[k] = v, _b), objFromEntries(xs));
    var _b;
}
exports.objFromEntries = objFromEntries;

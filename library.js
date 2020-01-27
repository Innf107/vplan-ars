"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __spreadArrays = (this && this.__spreadArrays) || function () {
    for (var s = 0, i = 0, il = arguments.length; i < il; i++) s += arguments[i].length;
    for (var r = Array(s), k = 0, i = 0; i < il; i++)
        for (var a = arguments[i], j = 0, jl = a.length; j < jl; j++, k++)
            r[k] = a[j];
    return r;
};
exports.__esModule = true;
var path = require("path");
function split(a, f) {
    if (a.length === 0)
        return [[], []];
    var x = a[0], xs = a.slice(1);
    var _a = split(xs, f), passed = _a[0], failed = _a[1];
    if (f(x))
        return [__spreadArrays([x], passed), failed];
    else
        return [passed, __spreadArrays([x], failed)];
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
function mergeObjWithFlat(a, b, f) {
    var entriesA = Object.entries(a);
    var entriesB = Object.entries(b);
    var mergedEntries = mergeByWithFlat(entriesA, entriesB, function (x, y) { return x[0] === y[0]; }, function (x, y) { return Object.entries(f(x, y)); });
    return objFromEntries(mergedEntries);
}
exports.mergeObjWithFlat = mergeObjWithFlat;
function mergeByWith(a, b, match, resolve) {
    if (a[0] === undefined)
        return b;
    if (b[0] === undefined)
        return a;
    var x = a[0], xs = a.slice(1);
    var _a = split(b, function (y) { return match(x, y); }), matched = _a[0], failed = _a[1];
    if (matched.length > 0) {
        return __spreadArrays([resolveAll(__spreadArrays([x], matched), resolve)], mergeByWith(xs, failed, match, resolve));
    }
    else
        return __spreadArrays([x], mergeByWith(xs, b, match, resolve));
}
exports.mergeByWith = mergeByWith;
function mergeByWithFlat(a, b, match, resolve) {
    if (a[0] === undefined)
        return b;
    if (b[0] === undefined)
        return a;
    var x = a[0], xs = a.slice(1);
    var _a = split(b, function (y) { return match(x, y); }), matched = _a[0], failed = _a[1];
    if (matched.length > 0) {
        return __spreadArrays(resolveAllFlat(__spreadArrays([x], matched), resolve), mergeByWithFlat(xs, failed, match, resolve));
    }
    else
        return __spreadArrays([x], mergeByWithFlat(xs, b, match, resolve));
}
exports.mergeByWithFlat = mergeByWithFlat;
function resolveAllFlat(a, resolve) {
    if (a.length == 0)
        return [];
    if (a.length == 1)
        return [a[0]];
    var x = a[0], y = a[1], xs = a.slice(2);
    return __spreadArrays(resolve(x, y), resolveAllFlat(__spreadArrays([y], xs), resolve));
}
exports.resolveAllFlat = resolveAllFlat;
function resolveAll(a, resolve) {
    return a.reduce(resolve);
}
exports.resolveAll = resolveAll;
exports.matchAll = function (rgx, str) {
    var match = str.match(rgx);
    if (match === null)
        return [];
    else {
        var remaining = str.replace(match[0], '');
        return __spreadArrays([match], exports.matchAll(rgx, remaining));
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
    var _a;
    if (list.length === 0)
        return {};
    var x = list[0], xs = list.slice(1);
    var _b = f(x), k = _b[0], v = _b[1];
    return __assign((_a = {}, _a[k] = v, _a), mapToObj(xs, f));
}
exports.mapToObj = mapToObj;
function objFromEntries(entries) {
    var _a;
    if (entries.length === 0)
        return {};
    var _b = entries[0], k = _b[0], v = _b[1], xs = entries.slice(1);
    return __assign((_a = {}, _a[k] = v, _a), objFromEntries(xs));
}
exports.objFromEntries = objFromEntries;
function flatten(xs) { return flatMap(xs, id); }
exports.flatten = flatten;
function flatMap(l, f) {
    if (l.length == 0)
        return [];
    var x = l[0], xs = l.slice(1);
    return __spreadArrays(f(x), flatMap(xs, f));
}
exports.flatMap = flatMap;
function id(x) { return x; }
exports.id = id;
function map(xs, f) {
    return xs.map(f);
}
exports.map = map;

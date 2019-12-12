"use strict";
exports.__esModule = true;
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

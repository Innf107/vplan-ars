"use strict";
exports.__esModule = true;
var XorShift128Plus = (function () {
    function XorShift128Plus(s01, s00, s11, s10) {
        this.s01 = s01;
        this.s00 = s00;
        this.s11 = s11;
        this.s10 = s10;
    }
    XorShift128Plus.prototype.min = function () {
        return -0x80000000;
    };
    XorShift128Plus.prototype.max = function () {
        return 0x7fffffff;
    };
    XorShift128Plus.prototype.next = function () {
        var a0 = this.s00 ^ (this.s00 << 23);
        var a1 = this.s01 ^ ((this.s01 << 23) | (this.s00 >>> 9));
        var b0 = a0 ^ this.s10 ^ ((a0 >>> 17) | (a1 << 15)) ^ ((this.s10 >>> 26) | (this.s11 << 6));
        var b1 = a1 ^ this.s11 ^ (a1 >>> 17) ^ (this.s11 >>> 26);
        return [(b0 + this.s10) | 0, new XorShift128Plus(this.s11, this.s10, b1, b0)];
    };
    return XorShift128Plus;
}());
exports.xorshift128plus = function (seed) {
    return new XorShift128Plus(-1, ~seed, 0, seed | 0);
};

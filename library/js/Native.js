const Native = {};

Native.eq = function(v1, v2) {
    return v1 === v2;
};

Native.multiply = function(v1, v2) {
    return v1 * v2;
};

Native.divide = function(v1, v2) {
    return v1 / v2;
};

Native.modulo = function(v1, v2) {
    return v1 % v2;
};

Native.plus = function(v1, v2) {
    return v1 + v2;
};

Native.minus = function(v1, v2) {
    return v1 - v2;
};

Native.concat = function(v1, v2) {
    return v1 + v2;
};

Native.negate = function(v) {
    return -v;
};

Native.size = function(v) {
    return v.length;
};

Native.isArray = function(v) {
    return typeof(v) === typeof([]);
};

Native.unsafeIndex = function(n, a) {
    return a[n];
};

Native.coerce = function(x) {
    return x;
};

Native.insert = function(a, e) {
    a.push(e);
    return a;
};

Native.toString = function(x) {
    return x.toString();
};

Native.print = function(x) {
    console.log(x);
};

Native.error = function(x) {
    throw x;
};

module.exports = Native
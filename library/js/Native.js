const Native = {};

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
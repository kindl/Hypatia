static var wildcard = function(){};

static var eq = function(v1)
{
    return function(v2)
    {
        return v1 == v2;
    };
};

static var times = function(v1)
{
    return function(v2)
    {
        return v1 * v2;
    };
};

static var divide = function(v1)
{
    return function(v2)
    {
        return v1 / v2;
    };
};

static var modulo = function(v1)
{
    return function(v2)
    {
        return v1 % v2;
    };
};

static var plus = function(v1)
{
    return function(v2)
    {
        return v1 + v2;
    };
};

static var minus = function(v1)
{
    return function(v2)
    {
        return v1 - v2;
    };
};

static var concat = function(v1)
{
    return function(v2)
    {
        return v1 + v2;
    };
};

static var negate = function(v)
{
    return -v;
};

static var size = function(v)
{
    return v.length;
};

static var isArray = function(v)
{
    return typeof(v) === typeof([]);
};

static var getn = function(n)
{
    return function(a : Object[])
    {
        return a[n - 1];
    };
};

static var primIf = function(val)
{
    return function(x)
    {
        return function(y)
        {
            if(val)
            {
                return x;
            }
            else
            {
                return y;
            };
        };
    };
};

static var coerce = function(x)
{
    return x;
};

static var insert = function(a)
{
    return function(e)
    {
        // TODO pushing to native arrays?
        a.Push(e);
        return a.ToBuiltin();
    };
};

static var empty = function(x)
{
    return [];
};

static var toString = function(x)
{
    return x.ToString();
};

static var write = function(x)
{
    Debug.Log(x);
    return null;
};

static var error = function(x)
{
    // TODO throw
    Debug.LogError(x);
    return null;
};
local utf8 = require("utf8")

local Native = {}

function Native.eq(v1)
    return function(v2)
        return v1 == v2
    end
end

function Native.lt(v1)
    return function(v2)
        return v1 < v2
    end
end

function Native.gt(v1)
    return function(v2)
        return v1 > v2
    end
end

function Native.le(v1)
    return function(v2)
        return v1 <= v2
    end
end

function Native.ge(v1)
    return function(v2)
        return v1 >= v2
    end
end

function Native.times(v1)
    return function(v2)
        return v1 * v2
    end
end

function Native.divide(v1)
    return function(v2)
        return v1 / v2
    end
end

function Native.modulo(v1)
    return function(v2)
        return v1 % v2
    end
end

function Native.plus(v1)
    return function(v2)
        return v1 + v2
    end
end

function Native.minus(v1)
    return function(v2)
        return v1 - v2
    end
end

function Native.power(v1)
    return function(v2)
        return v1 ^ v2
    end
end

function Native.concat(v1)
    return function(v2)
        return v1 .. v2
    end
end

function Native.negate(v)
    return -v
end

function Native.length(a)
    return #a
end

function Native.isArray(t)
    return type(t) == "table"
end

function Native.unsafeIndex(i)
    return function(a)
        return a[i + 1]
    end
end

function Native.unsafeCoerce(x)
    return x
end

function Native.unsafeInsert(a)
    return function(e)
        table.insert(a, e)
        return a
    end
end

function Native.unsafeSet(a)
    return function(i)
        return function(v)
            a[i + 1] = v
            return a
        end
    end
end

Native.uncurry2 = function(f)
    return function(a, b)
        return f(a)(b)
    end
end

Native.uncurry3 = function(f)
    return function(a,b,c)
        return f(a)(b)(c)
    end
end

Native.uncurry4 = function(f)
    return function(a,b,c,d)
        return f(a)(b)(c)(d)
    end
end

Native.uncurry5 = function(f)
    return function(a,b,c,d,e)
        return f(a)(b)(c)(d)(e)
    end
end

Native.True = true
Native.False = false

Native.sin = math.sin
Native.cos = math.cos
Native.print = print

Native.toString = tostring

Native.textLength = function(s)
    return utf8.len(s)
end

Native.substring = function(s)
    return function(i)
        return function(j)
            return utf8.char(utf8.codepoint(s, i + 1, j + 1))
        end
    end
end

Native.toNumber = function(s)
    return tonumber(s) or error("\"" .. s .. "\" is not a number")
end

Native.error = error

return Native

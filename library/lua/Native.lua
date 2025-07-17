local utf8 = require("utf8")

local Native = {}

Native.eq = function(v1, v2)
    return v1 == v2
end

Native.lt = function(v1, v2)
    return v1 < v2
end

Native.gt = function(v1, v2)
    return v1 > v2
end

Native.le = function(v1, v2)
    return v1 <= v2
end

Native.ge = function(v1, v2)
    return v1 >= v2
end

Native.multiply = function(v1, v2)
    return v1 * v2
end

Native.divide = function(v1, v2)
    return v1 / v2
end

Native.modulo = function(v1, v2)
    return v1 % v2
end

Native.plus = function(v1, v2)
    return v1 + v2
end

Native.minus = function(v1, v2)
    return v1 - v2
end

Native.power = function(v1, v2)
    return v1 ^ v2
end

Native.concat = function(v1, v2)
    return v1 .. v2
end

Native.negate = function(v)
    return -v
end

Native.length = function(a)
    return #a
end

Native.isArray = function(t)
    return type(t) == "table"
end

Native.unsafeIndex = function(i, a)
    return a[i + 1]
end

Native.unsafeCoerce = function(x)
    return x
end

Native.unsafeInsert = function(a, e)
    table.insert(a, e)
    return a
end

Native.unsafeSet = function(a, i, v)
    a[i + 1] = v
    return a
end

Native.True = true
Native.False = false

Native.sin = math.sin
Native.cos = math.cos
Native.tan = math.tan
Native.print = print

Native.toString = tostring

Native.textLength = function(s)
    return utf8.len(s)
end

Native.substring = function(s, startIndex, endIndex)
    local startOffset = utf8.offset(s, startIndex + 1)
    local endOffset = utf8.offset(s, endIndex + 2) - 1
    return string.sub(s, startOffset, endOffset)
end

Native.toNumber = function(s)
    return tonumber(s) or error("\"" .. s .. "\" is not a number")
end

Native.error = error

return Native

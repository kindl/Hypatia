local utf8 = require("utf8")

local Native = {}

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

Native.sin = math.sin
Native.cos = math.cos
Native.tan = math.tan
Native.print = print

Native.toString = tostring

Native.toLowerCase = function(t)
    string.lower(s)
end

Native.textLength = function(s)
    return utf8.len(s)
end

Native.slice = function(s, startIndex, endIndex)
    local startOffset = utf8.offset(s, startIndex + 1)
    local endOffset = utf8.offset(s, endIndex + 1)

    if startOffset == nil then
        return ""
    end

    if endOffset == nil then
        return string.sub(s, startOffset)
    end

    return string.sub(s, startOffset, endOffset - 1)
end

Native.toNumber = function(s)
    return tonumber(s) or error("\"" .. s .. "\" is not a number")
end

Native.error = error

return Native

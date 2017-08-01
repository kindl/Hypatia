local Native = {}

function Native.wildcard() end

function Native.eq(v1)
    return function(v2)
        return v1 == v2
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

function Native.split(seperator)
    return function(s)
        local result = {}
        for item in s:gmatch("([^"..seperator.."]*)") do
            table.insert(result, item)
        end
        return result
    end
end

function Native.polygon(line)
    love.graphics.polygon("line", line)
end

function Native.draw(img)
    return function(x)
        return function(y)
            love.graphics.draw(img, x, y)
        end
    end
end

function Native.size(a)
    return #a
end

function Native.isArray(t)
    return type(t) == "table"
end

function Native.getn(n)
    return function(a)
        local r = a[n]
        if r == nil
            then error("Indexed nil with key " .. tostring(n))
            else return r
        end
    end
end

function Native.primIf(val)
    return function(x)
        return function(y)
            if val then return x else return y end
        end
    end
end

function Native.coerce(x)
    return x
end

function Native.insert(a)
    return function(e)
        table.insert(a, e)
        return a
    end
end

function Native.getState()
    return state
end

function Native.setState(newState)
    state = newState
end

function Native.random(mini)
    return function(maxi)
        return love.math.random(mini, maxi)
    end
end

function Native.setColor(c)
    love.graphics.setColor(c[2], c[3], c[4])
end

function Native.circle(p)
    return function(r)
        love.graphics.circle("fill", p[2], p[3], r)
    end
end

Native.sin = math.sin
Native.cos = math.cos
Native.write = print
Native.file = love.filesystem.read
Native.image = love.graphics.newImage
Native.getWidth = love.graphics.getWidth
Native.getHeight = love.graphics.getHeight
Native.toString = tostring
Native.toNumber = function(s)
    r = tonumber(s)
    if r == nil then
        error("\"" .. s .. "\" is not a number")
    else
        return r
    end
end
Native.error = error

return Native

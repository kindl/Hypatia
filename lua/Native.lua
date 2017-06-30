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
        local l, r = s:match('(.-)' .. seperator ..'(.*)')
        if l == nil then
            return {}
        else
            return {l, r}
        end
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

function Native.get(i)
    return function(a)
        local r = a[i]
        if r == nil then error "Indexed nil" else return r end
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

function Native.empty()
    return {}
end

-- access to a global state
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
Native.print = print
Native.file = love.filesystem.read
Native.image = love.graphics.newImage
Native.getWidth = love.graphics.getWidth
Native.getHeight = love.graphics.getHeight
Native.toString = tostring
Native.toNumber = tonumber
Native.error = error

return Native

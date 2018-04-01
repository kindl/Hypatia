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
        local from
        local to
        from, to = s:find(seperator)
        if from == nil then
            return {s}
        else
            local result = s:sub(1, from - 1)
            local tail = s:sub(to + 1)
            local rest = Native.split(seperator)(tail)
            table.insert(rest, 1, result)
            return rest
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

function Native.geti(i)
    return function(a)
        return a[i + 1]
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

function Native.modifyRef(ref)
    return function(f)
        return function(s)
            ref[2] = f(ref[2])
        end
    end
end

function Native.random(mini)
    return function(maxi)
        return love.math.random(mini, maxi)
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
Native.write = print

Native.file = love.filesystem.read
Native.image = love.graphics.newImage
Native.getWidth = love.graphics.getWidth
Native.getHeight = love.graphics.getHeight

Native.setColor = function(c1)
    return function(c2)
        return function(c3)
            love.graphics.setColor(c1, c2, c3)
        end
    end
end

Native.circle = function(f)
    return function(x)
        return function(y)
            return function(r)
                love.graphics.circle(f, x, y, r)
            end
        end
    end
end

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

local Native_Love = {}

function Native_Love.polygon(line)
    love.graphics.polygon("line", line)
end

function Native_Love.draw(img)
    return function(x)
        return function(y)
            love.graphics.draw(img, x, y)
        end
    end
end

function Native_Love.random(mini)
    return function(maxi)
        return love.math.random(mini, maxi)
    end
end

Native_Love.file = love.filesystem.read
Native_Love.image = love.graphics.newImage
Native_Love.getWidth = love.graphics.getWidth
Native_Love.getHeight = love.graphics.getHeight

Native_Love.setColor = function(c1)
    return function(c2)
        return function(c3)
            love.graphics.setColor(c1, c2, c3)
        end
    end
end

Native_Love.circle = function(f)
    return function(x)
        return function(y)
            return function(r)
                love.graphics.circle(f, x, y, r)
            end
        end
    end
end

return Native_Love

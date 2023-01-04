local Native_Love = {}

function Native_Love.polygon(mode)
    return function (line)
        love.graphics.polygon(mode, line)
    end
end

function Native_Love.draw(img)
    return function(x)
        return function(y)
            love.graphics.draw(img, x, y)
        end
    end
end

function Native_Love.unsafeRandom(mini)
    return function(maxi)
        return love.math.random(mini, maxi)
    end
end

Native_Love.file = love.filesystem.read
Native_Love.newImage = love.graphics.newImage
Native_Love.getWidth = love.graphics.getWidth
Native_Love.getHeight = love.graphics.getHeight

Native_Love.imageWidth = function(image)
    return image:getWidth()
end

Native_Love.imageHeight = function(image)
    return image:getHeight()
end

Native_Love.newQuad = function(x)
    return function(y)
        return function(width)
            return function(height)
                return function(sw)
                    return function(sh)
                        return love.graphics.newQuad(x, y, width, height, sw, sh)
                    end
                end
            end
        end
    end
end

function Native_Love.drawImage(img)
    return function(quad)
        return function(x)
            return function(y)
                love.graphics.draw(img, quad, x, y)
            end
        end
    end
end

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

Native_Love.rectangle = function(f)
    return function(x)
        return function(y)
            return function(w)
                return function(h)
                    love.graphics.rectangle(f, x, y, w, h)
                end
            end
        end
    end
end

return Native_Love

local Native_Love = {}

function Native_Love.polygon(mode)
    return function (line)
        love.graphics.polygon(mode, line)
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

function Native_Love.drawImage(image)
    return function(x)
        return function(y)
            love.graphics.draw(image, x, y)
        end
    end
end

function Native_Love.drawQuad(texture)
    return function(quad)
        return function(x)
            return function(y)
                love.graphics.draw(texture, quad, x, y)
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

Native_Love.newShader = function(t)
    return love.graphics.newShader(t)
end

Native_Love.setShader = function(s)
    return love.graphics.setShader(s)
end

Native_Love.resetShader = function()
    return love.graphics.setShader()
end

Native_Love.setTexture = function(mesh)
    return function(texture)
        mesh:setTexture(texture)
    end
end

Native_Love.sendShader = function(shader)
    return function(key)
        return function(value)
            shader:send(key, value)
        end
    end
end

Native_Love.newMesh = function(format)
    return function(vertices)
        return function(mode)
            return love.graphics.newMesh(format, vertices, mode)
        end
    end
end

Native_Love.drawMesh = function(mesh)
    love.graphics.draw(mesh)
end

Native_Love.captureMouse = love.mouse.setRelativeMode

Native_Love.isDown = love.keyboard.isDown

return Native_Love

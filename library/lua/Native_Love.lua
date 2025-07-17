local Native_Love = {}

Native_Love.polygon = function(mode, line)
    love.graphics.polygon(mode, line)
end

Native_Love.unsafeRandom = function(minNumber, maxNumber)
    return love.math.random(minNumber, maxnumber)
end

Native_Love.file = love.filesystem.read

Native_Love.exists = function(path)
    -- Use getInfo because `love.filesystem.exists` is being deprecated
    return love.filesystem.getInfo(path) ~= nil
end

Native_Love.newImage = love.graphics.newImage
Native_Love.getWidth = love.graphics.getWidth
Native_Love.getHeight = love.graphics.getHeight

Native_Love.imageWidth = function(image)
    return image:getWidth()
end

Native_Love.imageHeight = function(image)
    return image:getHeight()
end

Native_Love.newQuad = function(x, y, width, height, sw, sh)
    return love.graphics.newQuad(x, y, width, height, sw, sh)
end

Native_Love.drawImage = function(image, x, y)
    love.graphics.draw(image, x, y)
end

Native_Love.drawQuad = function(texture, quad, x, y)
    love.graphics.draw(texture, quad, x, y)
end

Native_Love.setColor = function(c1, c2, c3)
    love.graphics.setColor(c1, c2, c3)
end

Native_Love.circle = function(f, x, y, r)
    love.graphics.circle(f, x, y, r)
end

Native_Love.rectangle = function(f, x, y, w, h)
    love.graphics.rectangle(f, x, y, w, h)
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

Native_Love.setTexture = function(mesh, texture)
    mesh:setTexture(texture)
end

Native_Love.sendShader = function(shader, key, value)
    shader:send(key, value)
end

Native_Love.newMesh = function(format, vertices, mode)
    return love.graphics.newMesh(format, vertices, mode)
end

Native_Love.drawMesh = function(mesh)
    love.graphics.draw(mesh)
end

Native_Love.captureMouse = love.mouse.setRelativeMode

Native_Love.isDown = love.keyboard.isDown

Native_Love.print = function(text)
    love.graphics.print(text)
end

return Native_Love

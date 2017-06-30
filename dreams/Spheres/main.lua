function drawSpheres()
    for _, sphere in pairs(spheres) do
        love.graphics.setColor(sphere.color)
        love.graphics.circle("fill", sphere.position[1], sphere.position[2], sphere.radius)
    end 
end

function makeSphere(position, radius, velocity, color)
    return {
        position = position,
        radius = radius,
        velocity = velocity,
        color = color
    }
end

function addSphere()
    local pos = {love.math.random(500), love.math.random(500)}
    local vel = {love.math.random(-100, 100), love.math.random(-100, 100)}
    local col = {love.math.random(0,255), love.math.random(0,255), love.math.random(0,255)}
    local rad = love.math.random(100)
    local sphere = makeSphere(pos, rad, vel, col)
    table.insert(spheres, sphere)
end

function love.load()
    spheres = {}
    
    for i=1,10 do
        addSphere()
        addSphere()
    end
end

function love.update(dt)
    for _, sphere in pairs(spheres) do
        sphere.position[1] = (sphere.position[1] + sphere.velocity[1] * dt) % love.graphics.getWidth()
        sphere.position[2] = (sphere.position[2] + sphere.velocity[2] * dt) % love.graphics.getHeight()
    end
end

function love.draw()
    drawSpheres()
end

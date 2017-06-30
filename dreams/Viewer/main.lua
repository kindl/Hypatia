local obj_loader = require "lua-obj/obj_loader"

local model = obj_loader.load("cube.obj")


function makeSimpleProjectionMatrix(c)
    return {
        {1,0,0,0},
        {0,1,0,0},
        {0,0,1,0},
        {0,0,-1/c,1},
    }
end

function makeScaleMatrix(f)
    return {
        {f, 0, 0, 0},
        {0, f, 0, 0},
        {0, 0, f, 0},
        {0, 0, 0, 1}
    }
end

function makeTranslateMatrix(x, y, z)
    return {
        {1, 0, 0, x},
        {0, 1, 0, y},
        {0, 0, 1, z},
        {0, 0, 0, 1}
    }
end

function makeRotationX(angle)
    local s = math.sin(angle)
    local c = math.cos(angle)
    return {
        {1, 0, 0, 0},
        {0, c,-s, 0},
        {0, s, c, 0},
        {0, 0, 0, 1},
    }
end

function makeRotationY(angle)
    local s = math.sin(angle)
    local c = math.cos(angle)
    return {
        { c, 0, s, 0},
        { 0, 1, 0, 0},
        {-s, 0, c, 0},
        { 0, 0, 0, 1},
    }
end

function makeRotationZ(angle)
    local s = math.sin(angle)
    local c = math.cos(angle)
    return {
        {c,-s, 0, 0},
        {s, c, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    }
end

function dot(v1, v2)
    return v1[1] * v2[1] + v1[2] * v2[2] + v1[3] * v2[3] + v1[4] * v2[4]
end

function length(vector)
    return math.sqrt(dot(vector, vector))
end

function normalize(vector)
    local l = length(vector)
    return {vector[1]/l, vector[2]/l, vector[3]/l, vector[4]/l}
end

function transpose(m)
    return {
        {m[1][1], m[2][1], m[3][1], m[4][1]},
        {m[1][2], m[2][2], m[3][2], m[4][2]},
        {m[1][3], m[2][3], m[3][3], m[4][3]},
        {m[1][4], m[2][4], m[3][4], m[4][4]},
    }
end

function multiply(m1, t)
    local m2 = transpose(t)
    return {
        {dot(m1[1],m2[1]), dot(m1[1],m2[2]), dot(m1[1],m2[3]), dot(m1[1],m2[4])},
        {dot(m1[2],m2[1]), dot(m1[2],m2[2]), dot(m1[2],m2[3]), dot(m1[2],m2[4])},
        {dot(m1[3],m2[1]), dot(m1[3],m2[2]), dot(m1[3],m2[3]), dot(m1[3],m2[4])},
        {dot(m1[4],m2[1]), dot(m1[4],m2[2]), dot(m1[4],m2[3]), dot(m1[4],m2[4])},
    }
end

function transform(m, v)
    return {dot(m[1], v), dot(m[2], v), dot(m[3], v), dot(m[4], v)}
end

function to2d(v)
    local w = v[4]
    return {
        v[1] / w,
        v[2] / w
    }
end

local screenWidth = 800
local screenHeight = 600
local t = makeTranslateMatrix(screenWidth / 2, screenHeight / 2, 0)

local s = makeScaleMatrix(screenHeight / 2)
local r = makeRotationZ(3.142)
local modelview = multiply(t, multiply(r, s))

local projection = makeSimpleProjectionMatrix(5000)

local lightDirection = normalize({0,0,1,0})

local faces = model.f
local vertices = model.v
local normals = model.vn

function love.draw()
    local rotation = makeRotationY(love.mouse.getX() / screenWidth)
    for _, face in pairs(faces) do
        local points = {}
        for _, point in pairs(face) do
            local vertex = vertices[point.v]
            local position = {vertex.x, vertex.y, vertex.z, 1}
            local rotated = transform(rotation, position)
            local transformed = transform(modelview, rotated)
            
            local projected = transform(projection, transformed)
            local divided = to2d(projected)
            
            table.insert(points, divided[1])
            table.insert(points, divided[2])
        end

        love.graphics.setColor(255,255,255)
        love.graphics.polygon("line", points)
    end
end

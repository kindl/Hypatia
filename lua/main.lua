local Game = require "Spheres"

local uncurry2 = function(f)
    return function(a, b)
        return f(a)(b)
    end 
end

local uncurry3 = function(f)
    return function(a,b,c)
        return f(a)(b)(c)
    end
end

local uncurry4 = function(f)
    return function(a,b,c,d)
        return f(a)(b)(c)(d)
    end
end

local uncurry5 = function(f)
    return function(a,b,c,d,e)
        return f(a)(b)(c)(d)(e)
    end
end

love.load = Game.load

love.draw = Game.draw

love.update = Game.update

love.mousemoved = uncurry5(Game.mousemoved)

love.mousepressed = uncurry4(Game.mousepressed)

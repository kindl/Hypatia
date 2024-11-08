local game = require "Spheres"

for k, _ in pairs(game) do
    love[k] = game[k]
end

local game = require "Example"

for k, _ in pairs(game) do
    love[k] = game[k]
end

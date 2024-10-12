local game = require "Example"

for k, _ in pairs(game) do
    if k ~= "update" then
        love[k] = game[k]
    end
end

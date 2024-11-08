local game = require "Viewer_Main"

for k, _ in pairs(game) do
    love[k] = game[k]
end

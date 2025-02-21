local game = require "Server_Main"

for k, _ in pairs(game) do
    love[k] = game[k]
end

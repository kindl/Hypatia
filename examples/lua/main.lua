local loadtime = 0
local filename = "Viewer_Main.lua"
local game

local watch = function(dt)
    -- Checks if the loaded game file is newer
    -- NOTE getLastModified will be deprecated with love 0.11
    local modtime = love.filesystem.getLastModified(filename)
    if modtime > loadtime then
        game = love.filesystem.load(filename)()
        if game then
            for k, _ in pairs(game) do
                if k ~= "update" then
                    love[k] = game[k]
                end
            end
            if game.load then game.load() end
            loadtime = modtime
            print("Loaded " .. filename)
        else
            print("Cannot load " .. filename)
        end
    else
        if game.update then game.update(dt) end
    end
end

love.update = watch

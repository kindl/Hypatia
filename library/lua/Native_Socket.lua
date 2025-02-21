local socket = require("socket")


local Closed = function() end
local Timeout = function() end
local Message
Message = function(text)
    return {Message, text}
end

local connect = function(address)
    return function(port)
        local client = socket.connect(address, port)
        assert(client)
        return client
    end
end

local bind = function(address)
    return function(port)
        local server = socket.bind(address, port)
        assert(server)
        return server
    end
end

local accept = function(server)
    local connection = server:accept()
    assert(connection)
    return connection
end

local receive = function(connection)
    local received, error = connection:receive()
    if received ~= nil then
        return Message(received)
    elseif error == "closed" then
        return Closed
    else
        return Timeout
    end
end

local send = function(connection)
    return function(message)
        connection:send(message)
    end
end

local close = function(connection)
    connection:close()
end

return {
    Closed = Closed,
    Timeout = Timeout,
    Message = Message,
    connect = connect,
    bind = bind,
    accept = accept,
    receive = receive,
    send = send,
    close = close
}
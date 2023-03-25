local socket = require("socket")

Native_Socket = {}

Native_Socket.tcpConnect = function(host)
    return function(port)
        local tcp = socket.tcp()
        assert(tcp)
        tcp:connect(host, port)
        return tcp
    end
end

Native_Socket.send = function(tcp)
    return function(message)
        tcp:send(message)
    end
end

Native_Socket.close = function(tcp)
    tcp:close()
end

return Native_Socket
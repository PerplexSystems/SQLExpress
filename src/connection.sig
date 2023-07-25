structure Connection = struct
  type connection = (INetSock.inet, Socket.active Socket.stream) Socket.sock
  datatype t =
    Success of connection
  | Failure of string
end

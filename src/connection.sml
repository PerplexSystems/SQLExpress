structure PostgresClient = struct
    fun connect () =
        let 
            val socket: Socket.active INetSock.stream_sock = INetSock.TCP.socket()
            val SOME postgres = NetHostDB.getByName "127.0.0.1"
            val addr = INetSock.toAddr(NetHostDB.addr postgres, 5432)
        in
            Socket.connect (socket, addr);
            socket
        end

    fun read (socket: Socket.active INetSock.stream_sock) =
        case Socket.select{rds = [Socket.sockDesc socket], wrs=[], exs=[], timeout=SOME(Time.fromSeconds 10)} of
            { rds = [], ...} => ()
        |  _ => let val text = Socket.recvVec(socket, 1)
                in if Word8Vector.length text = 0 then ()
                        else ( print(Byte.bytesToString text); read socket )
                end

    fun startup (socket: Socket.active INetSock.stream_sock) =
        let 
            val message = "user\\0postgres\\0database\\0postgres\\0postgres\\0\\0"
            val length = Word8Vector.fromList [ Word8.fromInt (String.size message) ]
            val protocolVersion = Word8Vector.fromList [Word8.fromInt 196608]
            val serializedMessage = Byte.stringToBytes message
            val buffer = Word8VectorSlice.full (Word8Vector.concat [length, protocolVersion, serializedMessage])
            (* val socket = connect() *)
            val _ = Socket.sendVec(socket, buffer)
            val () = read socket
        in 
            Socket.close socket
        end
end

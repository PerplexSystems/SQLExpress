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

    fun convertString (elem: string) =
        (Word8Vector.fromList (List.rev (List.map Byte.charToByte (String.explode elem))))

    fun convertInt (n: int) =
        Word8Vector.fromList
            [ Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx24)), 0wxFF),
              Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx16)), 0wxFF),
              Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx8)), 0wxFF),
              Word8.andb (Word8.fromInt n, 0wxFF) ]

    fun startup (socket: Socket.active INetSock.stream_sock) =
        let 
            val message = convertString "user\000postgres\000database\000postgres\000postgres\000\000"
            val length = 
                convertInt 50
                (* Word8Vector.fromList [ 0wx50, 0wx0, 0wx0, 0wx0 ] *)
            val protocolVersion = 
                Word8Vector.fromList [0w0, 0w3, 0w0, 0w0]
            val buffer = Word8VectorSlice.full (Word8Vector.concat [ length, protocolVersion, message ])
            (* val buffer = Word8VectorSlice.full (Word8Vector.concat [length, protocolVersion, serializedMessage]) *)
            (* val socket = connect() *)
            val buf = PolyML.makestring protocolVersion
        in 
            print buf;
            Socket.sendVec(socket, buffer);
            read socket;
            
            Socket.close socket
        end
end

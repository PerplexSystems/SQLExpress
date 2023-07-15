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

    fun convertString (elem: string) =
        (Word8Vector.fromList (List.map Byte.charToByte (String.explode elem)))

    fun intToBytes (n: int) =
        Word8Vector.fromList
            [ Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx24)), 0wxFF),
              Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx16)), 0wxFF),
              Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx8)), 0wxFF),
              Word8.andb (Word8.fromInt n, 0wxFF) ]

    fun bytesToInt (bytes: Word8Vector.vector) =
        let
            open Word8
            open Word8VectorSlice
            val slices = full bytes
            val first = sub(slices, 0)
            val second = sub(slices, 1)
            val third = sub(slices, 2)
            val fourth = sub(slices, 3)
        in
            Word8.toInt (orb (fourth, (orb ((<< (third, 0wx8)), (orb ((<< (second, 0wx16)), (<< (first, 0wx24))))))))
        end

    fun startup (socket: Socket.active INetSock.stream_sock) =
        let 
            val message = convertString "user\000admin\000database\000socket\000\000"
            val length = 
                intToBytes 36
                (* Word8Vector.fromList [ 0wx50, 0wx0, 0wx0, 0wx0 ] *)
            val protocolVersion = 
                Word8Vector.fromList [0w0, 0w3, 0w0, 0w0]
            val buffer = Word8VectorSlice.full (Word8Vector.concat [ length, protocolVersion, message ])
            val buf = PolyML.makestring buffer
        in 
            print buf;
            print "\n\n";

            Socket.sendVec(socket, buffer)
        end

        fun parser (socket: Socket.active INetSock.stream_sock) =
            let
                val messageType = Byte.bytesToString (Socket.recvVec(socket, 1))
                val length =  
                    bytesToInt(Socket.recvVec(socket, 4))
                val mP =  PolyML.makestring messageType
                val lP =  PolyML.makestring length
            in 
                case messageType of
                    "R" => print "Password"
                  | "Z" => print "ReadyForQuery"
                  | "E" => print "Error"
                  | _ => print "NAY";
                Socket.close socket
            end
end

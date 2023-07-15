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
            (* val buffer = Word8VectorSlice.full (Word8Vector.concat [length, protocolVersion, serializedMessage]) *)
            (* val socket = connect() *)
            (* val buffer2 =  *)
                (* Hardcoded but correct startup message *)
                (* Word8VectorSlice.full *)
                    (* (Word8Vector.fromList *)
                        (* [ 0wx00, 0wx00, 0wx00, 0wx24, 0wx00, 0wx03, 0wx00, 0wx00, 0wx75, 0wx73, 0wx65, 0wx72, 0wx00, 0wx61, 0wx64, 0wx6d, 0wx69, 0wx6e, 0wx00, 0wx64, 0wx61, 0wx74, 0wx61, 0wx62, 0wx61, 0wx73, 0wx65, 0wx00, 0wx73, 0wx6f, 0wx63, 0wx6b, 0wx65, 0wx74, 0wx00, 0wx00 ]) *)
            val buf = PolyML.makestring buffer
            (* val buf2 = PolyML.makestring buffer2 *)
        in 
            print buf;
            print "\n\n";
            (* print buf2; *)

            Socket.sendVec(socket, buffer)
            (* read socket; *)
            
            (* Socket.close socket *)
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

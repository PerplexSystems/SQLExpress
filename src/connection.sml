(* https://www.postgresql.org/docs/current/protocol-message-formats.html *)

structure PostgresClient = struct

    datatype 't Operation = OK | ERROR of 't

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

        fun password (message: Word8Vector.vector) =
            
            let 
                val (vec, _, _) = Word8VectorSlice.base (Word8VectorSlice.slice (message, 0, SOME 4))
            in 
                case (bytesToInt vec) of
                  0 => OK
                | 5 => ERROR "MD5 method not implemented."
                | _ => ERROR "Unindentified password method."
            end
            
            (* let 
                val inner = 
                    MD5.final (MD5.update(MD5.init, convertString "admin\000admin"))
                val pwd =
                    Word8Vector.concat
                        [(convertString "md5"),
                         (MD5.final (MD5.update (MD5.init, (Word8Vector.concat [ inner, convertString salt ]))))]
                val serialized = PolyML.makestring pwd
            in 
                print serialized
            end *)

        fun execute (socket: Socket.active INetSock.stream_sock) query =
            let 
                val queryFlag = 
                    Word8VectorSlice.full (Word8Vector.fromList [Byte.charToByte #"Q"])
                val serializedQuery = 
                    Word8VectorSlice.full (convertString query)
                val length = intToBytes (1 + 4 + Word8VectorSlice.length serializedQuery)
                val message = Word8VectorSlice.full (Word8VectorSlice.concat [queryFlag, Word8VectorSlice.full length, serializedQuery])
                val str = PolyML.makestring message
            in  print str;
                Socket.sendVec (socket, message);
                OK
            end 

        fun parser (socket: Socket.active INetSock.stream_sock) =
            let
                val messageType = 
                    Byte.bytesToString (Socket.recvVec(socket, 1))
                val length =  
                    bytesToInt(Socket.recvVec(socket, 4))
                val message = Socket.recvVec(socket, length - 5)
                val mP =  "Type: " ^ PolyML.makestring messageType
                val lP = PolyML.makestring length
                val messagePrint = PolyML.makestring message
                val pwd = convertString "admin"
                val x = Word8VectorSlice.full (Word8Vector.concat [Word8Vector.fromList [Byte.charToByte #"p"], intToBytes (Word8Vector.length pwd + 1 + 4), pwd])
            in 
                print "\n\n";
                print messagePrint;
                print "\n\n";
                print mP;
                print "\n\n";
                case messageType of
                    "R" =>
                    (print "Authenticating";
                     password (Socket.recvVec(socket, length)))
                  | _ => 
                    (print "Ready for query";
                     execute socket "SELECT 'Hello :)'")
                  (* | "E" => ERROR "Error" *)
                  (* | "p" => (Socket.sendVec(socket, x); ERROR "Password response") *)
                  (* | "\^V" => (print lP; OK) *)
                  (* | _ => ERROR "NAY" *)
            end
end

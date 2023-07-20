(* https://www.postgresql.org/docs/current/protocol-message-formats.html *)

structure PostgresClient =
struct

  datatype 't Operation = OK | ERROR of 't

  fun connect () =
    let
      val socket: Socket.active INetSock.stream_sock = INetSock.TCP.socket ()
      val SOME postgres = NetHostDB.getByName "127.0.0.1"
      val addr = INetSock.toAddr (NetHostDB.addr postgres, 5432)
    in
      Socket.connect (socket, addr);
      socket
    end

  fun convertString (elem: string) =
    (Word8Vector.fromList (List.map Byte.charToByte (String.explode elem)))

  fun intToBytes (n: int) =
    Word8Vector.fromList
      [ Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx24)), 0wxFF)
      , Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx16)), 0wxFF)
      , Word8.andb ((Word8.>> ((Word8.fromInt n), 0wx8)), 0wxFF)
      , Word8.andb (Word8.fromInt n, 0wxFF)
      ]

  fun bytesToInt (bytes: Word8Vector.vector) =
    let
      open Word8
      open Word8VectorSlice
      val slices = full bytes
      val first = sub (slices, 0)
      val second = sub (slices, 1)
      val third = sub (slices, 2)
      val fourth = sub (slices, 3)
    in
      Word8.toInt (orb (fourth, (orb ((<<(third, 0wx8)), (orb
        ((<<(second, 0wx16)), (<<(first, 0wx24))))))))
    end

  fun bytesToInt16 (bytes: Word8Vector.vector) =
    let
      open Word8
      open Word8VectorSlice
      val slices = full bytes
      val first = sub (slices, 0)
      val second = sub (slices, 1)
    in
      Word8.toInt (orb (second, (<<(first, 0wx8))))
    end

  fun startup (socket: Socket.active INetSock.stream_sock) =
    let
      val message = convertString "user\000admin\000database\000socket\000\000"
      val length = intToBytes 36
      (* Word8Vector.fromList [ 0wx50, 0wx0, 0wx0, 0wx0 ] *)
      val protocolVersion = Word8Vector.fromList [0w0, 0w3, 0w0, 0w0]
      val buffer = Word8VectorSlice.full
        (Word8Vector.concat [length, protocolVersion, message])
    (* val buf = PolyML.makestring buffer *)
    in
      (* print buf; *)
      (* print "\n\n"; *)

      Socket.sendVec (socket, buffer)
    end

  fun password (message: Word8Vector.vector) =

    let
      val (vec, _, _) = Word8VectorSlice.base
        (Word8VectorSlice.slice (message, 0, SOME 4))
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
      val queryFlag = Word8VectorSlice.full
        (Word8Vector.fromList [Byte.charToByte #"Q"])
      val serializedQuery = Word8VectorSlice.full (convertString query)
      val length = intToBytes (4 + 1 + Word8VectorSlice.length serializedQuery)
      val message = Word8VectorSlice.full (Word8VectorSlice.concat
        [ queryFlag
        , Word8VectorSlice.full length
        , serializedQuery
        , Word8VectorSlice.full (convertString "\000")
        ])
      val str = PolyML.makestring message
    in
      print str;
      Socket.sendVec (socket, message);
      OK
    end

  type Retrieval = {name: string, sqlType: string, records: string list}

  fun parser (socket: Socket.active INetSock.stream_sock) (dic: Retrieval list) =
    let
      val messageType = (* Byte.bytesToString ( *) Socket.recvVec (socket, 1)
      val length = (* bytesToInt( *) Socket.recvVec (socket, 4)
      val mP =
        "Type: " ^ PolyML.makestring messageType ^ " -> "
        ^ (Byte.bytesToString messageType)
      val lP =
        "Length: " ^ PolyML.makestring length ^ " -> "
        ^ (Int.toString (bytesToInt length))
      fun convertHeaders contents _ =
        let
          val reference = !contents
        in
          case Word8VectorSlice.findi (fn (_, byte) => byte = 0wx0) (!contents) of
            SOME (index, _) =>
              (* parser socket (Dictionary.assoc (Byte.bytesToString (Word8VectorSlice.concat [Word8VectorSlice.subslice ((!contents), 0, SOME index)])) [] dic) *)
              ( contents
                :=
                Word8VectorSlice.subslice
                  ( !contents
                  , index
                    +
                    19 (* integers in the message for this field + 1 NULL byte *)
                  , NONE
                  )
              ; { name = Byte.bytesToString (Word8VectorSlice.concat
                    [Word8VectorSlice.subslice (reference, 0, SOME index)])
                , sqlType = "X"
                , records = []
                }
              )
          | NONE => raise Fail "Could not find null terminated"
        (* ; print ("FIELD: " ^ (!fieldName) ^ "\n") *)
        end
      val dicString = PolyML.makestring dic
    in
      print ("----------->" ^ dicString);
      print "\n\n";
      print mP;
      print "\n\n";
      print lP;
      print "\n\n";
      case (Byte.bytesToString messageType) of
        "R" =>
          ( print "Authenticating\n"
          ; password (Socket.recvVec (socket, (bytesToInt length) - 4))
          ; Posix.Process.sleep (Time.fromSeconds 1)
          ; parser socket dic
          )
      | "Z" =>
          ( print "ReadyForQuery\n"
          ; Posix.Process.sleep (Time.fromSeconds 1)
          ; print
              ("Transaction status: "
               ^ (Byte.bytesToString (Socket.recvVec (socket, 1))) ^ "\n")
          )
      | "E" =>
          ( print
              ("ERROR: "
               ^
               (Byte.bytesToString (Socket.recvVec
                  (socket, (bytesToInt length) - 4))) ^ "\n")
          ; Posix.Process.sleep (Time.fromSeconds 1)
          ; parser socket dic
          )
      | "S" =>
          ( print "Syncing...\n"
          ; Socket.recvVec (socket, (bytesToInt length) - 4)
          ; (* Posix.Process.sleep (Time.fromSeconds 1); *) parser socket dic
          )
      | "K" =>
          ( print "BackendKeyData\n"
          ; Socket.recvVec (socket, (bytesToInt length) - 4)
          ; parser socket dic
          )
      | "T" =>
          (let
             val numberOfHeaders = bytesToInt16 (Socket.recvVec (socket, 2))
             val contents = ref (Word8VectorSlice.full (Socket.recvVec
               (socket, (bytesToInt length) - 4 - 2)))
             val fieldName = ref ""
           in
             print ("Receiving " ^ Int.toString numberOfHeaders ^ " headers\n");
             if numberOfHeaders <> 0 then
               parser socket
                 (List.tabulate (numberOfHeaders, convertHeaders contents))
             else
               print "Nothing :(\n"
           end
           handle SysErr => print "SYS\n")
      | "D" =>
          let
            val numberOfColumns = bytesToInt16 (Socket.recvVec (socket, 2))
            val contents = ref (Word8VectorSlice.full (Socket.recvVec
              (socket, (bytesToInt length) - 4 - 2)))
            val firstSize = bytesToInt (Word8VectorSlice.concat
              [Word8VectorSlice.subslice ((!contents), 0, SOME 4)])
            val fieldContent = ref ""
          in
            contents := Word8VectorSlice.subslice ((!contents), 4, NONE);
            print ("Receiving " ^ Int.toString numberOfColumns ^ " columns\n");
            if numberOfColumns <> 0 then
              case firstSize of
                ~1 => fieldContent := "NULL"
              | n =>
                  ( print ("SIZE: " ^ Int.toString firstSize ^ "\n")
                  ; (* fieldContent := Byte.bytesToString (Word8VectorSlice.concat [ !contents]) *)
                    fieldContent
                    :=
                    Byte.bytesToString (Word8VectorSlice.concat
                      [Word8VectorSlice.subslice ((!contents), 0, SOME n)])
                  (* parser socket (Dictionary.assoc (Byte.bytesToString (Word8VectorSlice.concat [Word8VectorSlice.subslice ((!contents), 0, SOME index)])) [] dic) *)
                  )
            else
              print "Nothing :(\n";
            print ("Received: " ^ (!fieldContent))
          end
      | _ =>
          ( print "Ignoring"
          ; Socket.recvVec (socket, (bytesToInt length) - 4)
          ; Posix.Process.sleep (Time.fromSeconds 1)
          ; parser socket dic
          )
    end
end

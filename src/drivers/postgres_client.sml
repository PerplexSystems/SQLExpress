(* https://www.postgresql.org/docs/current/protocol-message-formats.html *)

structure PostgresClient: VENDOR_DRIVER =
struct

  infix 3 <|
  fun f <| x = f x
  infix 3 |>
  fun y |> f = f y
  infix 3 <||
  fun f <|| (x, y) = f (x, y) (* Left section      *)
  infix 3 ||>
  fun (x, y) ||> f = f (x, y) (* Left application  *)

  datatype SQLType
    = Varchar
    | Date4

  val postgresTypeTable =
    (* TODO: Refactor later with a proper hashmap *)
    let val table = []
    in (
      (1082, Date4)::(1043, Varchar)::table      
    )
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

  fun bytesToInt32 (bytes: Word8Vector.vector) =
    let
      open Word8
      open Word8VectorSlice
      val slices = full bytes
      val first = Word32.fromLargeWord (Word8.toLargeWord (sub (slices, 0)))
      val second = Word32.fromLargeWord (Word8.toLargeWord (sub (slices, 1)))
      val third = Word32.fromLargeWord (Word8.toLargeWord (sub (slices, 2)))
      val fourth = Word32.fromLargeWord (Word8.toLargeWord (sub (slices, 3)))
    in
      case
        ( Word32.compare (first, 0wxFF)
        , Word32.compare (second, 0wxFF)
        , Word32.compare (third, 0wxFF)
        , Word32.compare (fourth, 0wxFF)
        )
      of
        (EQUAL, EQUAL, EQUAL, EQUAL) => ~1
      | _ =>
          Word32.toInt
            (Word32.orb
               ( fourth
               , (Word32.orb ((Word32.<< (third, 0wx8)), (Word32.orb
                    ((Word32.<< (second, 0wx16)), (Word32.<< (first, 0wx24))))))
               ))
    end

  fun bytesToInt16 (bytes: Word8Vector.vector) =
    let
      open Word8
      open Word8VectorSlice
      val slices = full bytes
      val first = Word16.fromLargeWord (Word8.toLargeWord (sub (slices, 0)))
      val second = Word16.fromLargeWord (Word8.toLargeWord (sub (slices, 1)))
    in
      case (Word16.compare (first, 0wxFF), Word16.compare (second, 0wxFF)) of
        (EQUAL, EQUAL) => ~1
      | _ => Word16.toInt (Word16.orb (second, (Word16.<< (first, 0wx8))))
    end

  fun startup (user, database, conn: Connection.connection) =
    let
      val message = convertString
        ("user\000" ^ user ^ "\000database\000" ^ database ^ "\000\000")
      val length = intToBytes ((Word8Vector.length message) + 8)
      val protocolVersion = Word8Vector.fromList [0w0, 0w3, 0w0, 0w0]
      val buffer = Word8VectorSlice.full
        (Word8Vector.concat [length, protocolVersion, message])
      val _ = Socket.sendVec (conn, buffer)
    in
      ()
    end

  fun handlePassword (message: Word8Vector.vector) =

    let
      val (vec, _, _) = Word8VectorSlice.base
        (Word8VectorSlice.slice (message, 0, SOME 4))
    in
      case (bytesToInt32 vec) of
        0 => ()
      | 5 => raise Fail "MD5 method not implemented."
      | _ => raise Fail "Unindentified password method."
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

  type Metadata = {transactionStatus: string}
  val metadata: Metadata option ref = ref NONE

  fun authenticateAndSync (conn: Connection.connection) =
    let
      val messageType = Socket.recvVec (conn, 1)
      val length = Socket.recvVec (conn, 4)
    in
      case (Byte.bytesToString messageType) of
        "R" =>
          ( handlePassword (Socket.recvVec (conn, (bytesToInt32 length) - 4))
          ; authenticateAndSync conn
          )
      | "S" =>
          ( Socket.recvVec (conn, (bytesToInt32 length) - 4)
          ; authenticateAndSync conn
          )
      | "K" =>
          ( Socket.recvVec (conn, (bytesToInt32 length) - 4)
          ; authenticateAndSync conn
          )
      | "Z" =>
          ( metadata
            :=
            SOME
              {transactionStatus = Byte.bytesToString (Socket.recvVec (conn, 1))}
          )
      | _ => raise (Fail "Out of sync, aborting connection.")
    end

  fun connect (user: string, database: string, host: string, port: int) :
    Connection.t =
    let
      val conn: Socket.active INetSock.stream_sock = INetSock.TCP.socket ()
    in
      case (NetHostDB.getByName host) of
        SOME remote =>
          ( (conn, INetSock.toAddr (NetHostDB.addr remote, port))
            ||> Socket.connect
          ; startup (user, database, conn)
          ; authenticateAndSync conn
          ; Connection.Success conn
          )
      | NONE => Connection.Failure ("Could not find supplied host: " ^ host)
    end
    handle ex => Connection.Failure (exnMessage ex)

  datatype TypeSize =
      Variable
    | Fixed of int
  type QueryResult = 
    { name: string,
      sqlTypeId: int,
      records: string list,
      tableObjectId: int,
      column: int,
      sqlTypeSize: TypeSize }
  val results: QueryResult list ref = ref []

  fun display () =
    let 
      val str = PolyML.makestring (!results)
    in print (str ^ "\n")
    end

  fun debug something =
    let 
      val str = PolyML.makestring something
    in print (str ^ "\n")
    end

  fun getType identifier =
    HashTable.find postgresTypeTable identifier

  fun fetch (conn: Connection.connection) =
    let
      val messageType = Socket.recvVec (conn, 1)
      val length = Socket.recvVec (conn, 4)
      fun convertHeaders contents _ =
        let
          val reference = !contents
          val foundIndex = 
            Word8VectorSlice.findi (fn (_, byte) => byte = 0wx0) reference
            |> Option.map (fn (index, _) => index)
          fun composeType initialIndex =
            let 
              val composition: QueryResult = 
                { name = Byte.bytesToString (Word8VectorSlice.concat [Word8VectorSlice.subslice (reference, 0, SOME initialIndex)])
                , tableObjectId = bytesToInt32 (Word8VectorSlice.concat [Word8VectorSlice.subslice (reference, initialIndex + 1, SOME 4)])
                , records = []
                , column = bytesToInt16 (Word8VectorSlice.concat [Word8VectorSlice.subslice (reference, initialIndex + 5, SOME 2)])
                , sqlTypeId = bytesToInt32 (Word8VectorSlice.concat [Word8VectorSlice.subslice (reference, initialIndex + 7, SOME 4)])
                , sqlTypeSize = 
                    let val sqlTypeSize = bytesToInt16 (Word8VectorSlice.concat [Word8VectorSlice.subslice (reference, initialIndex + 11, SOME 2)])
                    in if sqlTypeSize < 0
                    then Variable
                    else Fixed sqlTypeSize
                    end }
            in composition
            end
        in
          case foundIndex of
            SOME stringIndex => (
              contents
                := Word8VectorSlice.subslice
                  ( !contents
                  , stringIndex
                    +
                    19 (* integers in the message for this field + 1 NULL byte *)
                  , NONE
                  );
              composeType stringIndex
            )
          | NONE => raise Fail "Could not find null terminator"
        end
      fun convertData (elem: QueryResult) contents : QueryResult =
        let
          val reference = !contents
          val size = bytesToInt32 (Word8VectorSlice.concat
            [Word8VectorSlice.subslice (!contents, 0, SOME 4)])
        in
          case size of
            ~1 =>
              ( contents := Word8VectorSlice.subslice (!contents, 4, NONE)
              ; { name = (#name elem)
                , sqlTypeId = (#sqlTypeId elem)
                , records = "NULL" :: (#records elem)
                , column = (#column elem)
                , tableObjectId = (#tableObjectId elem)
                , sqlTypeSize = (#sqlTypeSize elem) }
              )
          | n =>
              ( contents := Word8VectorSlice.subslice (!contents, 4 + n, NONE)
              ; { name = (#name elem)
                , sqlTypeId = (#sqlTypeId elem)
                , records =
                    (Byte.bytesToString (Word8VectorSlice.concat
                       [Word8VectorSlice.subslice (reference, 4, SOME n)]))
                    :: (#records elem)
                , column = (#column elem)
                , tableObjectId = (#tableObjectId elem)
                , sqlTypeSize = (#sqlTypeSize elem) }
              )
        end
    in
      case (Byte.bytesToString messageType) of
        "Z" =>
          ( metadata
            :=
            SOME
              {transactionStatus = Byte.bytesToString (Socket.recvVec (conn, 1))}
          )
      | "E" =>
          ( print
              ("ERROR: "
               ^
               (Byte.bytesToString (Socket.recvVec
                  (conn, (bytesToInt32 length) - 4))) ^ "\n")
          ; fetch conn
          )
      | "T" =>
          let
            val numberOfHeaders = bytesToInt16 (Socket.recvVec (conn, 2))
            val contents = ref (Word8VectorSlice.full (Socket.recvVec
              (conn, (bytesToInt32 length) - 4 - 2)))
          in
            if numberOfHeaders <> 0 then
              ( results
                := (List.tabulate (numberOfHeaders, convertHeaders contents))
              ; fetch conn
              )
            else
              raise Fail "Could not fetch header for results"
          end
      | "D" =>
          let
            val numberOfColumns = bytesToInt16 (Socket.recvVec (conn, 2))
            val contents = ref (Word8VectorSlice.full (Socket.recvVec
              (conn, (bytesToInt32 length) - 4 - 2)))
          in
            if numberOfColumns <> 0 then
              ( results
                :=
                (List.map (fn (elem: QueryResult) => convertData elem contents)
                   (!results))
              ; fetch conn
              )
            else
              ()
          end
      | "C" => (* Message close after a query *)
          ( Socket.recvVec (conn, (bytesToInt32 length) - 4)
          ; fetch conn
          )
      | otherwise =>
          ( Socket.recvVec (conn, (bytesToInt32 length) - 4)
          ; raise Fail
              ("Messages expected to come in order, received: " ^ otherwise)
          )
    end

  fun execute (conn: Connection.connection, query) =
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
    in
      Socket.sendVec (conn, message);
      fetch conn
    end

  fun close (conn: Connection.connection) =
    Socket.close conn
    handle _ => () (* Already closed *)

  fun query (conn: Connection.connection, queryString: string): QueryResult list = (
    execute(conn, queryString);
    !results
  )
end

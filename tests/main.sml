fun main () =
  let
    val conn = PostgresClient.connect ("admin", "socket", "127.0.0.1", 5432)
  in
    case conn of
      Connection.Success conn =>
        ( PostgresClient.execute
            ( conn
            , "SELECT NULL AS First, 'World! :)' AS Second, NULL AS Third;"
            )
        ; PostgresClient.display ()
        ; PostgresClient.execute
            (conn, "SELECT 'See ya!' AS First, ':)' AS Second;")
        ; PostgresClient.display ()
        ; PostgresClient.close conn
        )
    | Connection.Failure message => print message
  end

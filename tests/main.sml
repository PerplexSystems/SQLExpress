fun main () =
  let
    val conn = PostgresClient.connect ("admin", "socket", "127.0.0.1", 5432)
  in
    case conn of
      Connection.Success conn =>
        ( PostgresClient.execute
            ( conn
            , "CREATE TABLE editors(name VARCHAR(20) NOT NULL PRIMARY KEY, language VARCHAR(20) NOT NULL);"
            )
        ; PostgresClient.execute
            (conn, "INSERT INTO editors(name, language) VALUES ('GNU/Emacs', 'Emacs Lisp'), ('Vim', 'Vimscript');")
        ; PostgresClient.execute
            (conn, "SELECT * FROM editors;")
        ; PostgresClient.display ()
        ; PostgresClient.close conn
        )
    | Connection.Failure message => print message
  end

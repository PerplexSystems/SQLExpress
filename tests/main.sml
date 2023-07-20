fun main _ =
  let
    val socket = PostgresClient.connect ()
  in
    PostgresClient.startup socket;
    PostgresClient.parser socket [];
    PostgresClient.execute socket
      "SELECT 'Hello' AS First, 'World! :)' AS Second;";
    PostgresClient.parser socket [];
    PostgresClient.parser socket [];
    Socket.close socket
  end

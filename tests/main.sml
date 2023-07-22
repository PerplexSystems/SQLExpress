fun main _ =
  let
    val socket = PostgresClient.connect ()
  in
    PostgresClient.startup socket;
    PostgresClient.parser socket;
    PostgresClient.execute socket "SELECT NULL AS First, 'World! :)' AS Second;";
    PostgresClient.parser socket;
    PostgresClient.display ();
    PostgresClient.execute socket "SELECT 'See ya!' AS First, ':)' AS Second;";
    PostgresClient.parser socket;
    PostgresClient.display ();
    Socket.close socket
  end

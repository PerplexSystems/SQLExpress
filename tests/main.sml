fun main _ =
    let 
      val socket = PostgresClient.connect()
    in
      PostgresClient.startup socket;
      PostgresClient.parser socket;
      PostgresClient.parser socket;
      (* Posix.Process.sleep  (Time.fromSeconds 3); *)
      Socket.close socket
    end
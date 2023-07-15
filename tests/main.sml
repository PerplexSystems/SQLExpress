fun main _ =
    let 
      val socket = PostgresClient.connect()
    in
      PostgresClient.startup socket;
      PostgresClient.parser socket
    end
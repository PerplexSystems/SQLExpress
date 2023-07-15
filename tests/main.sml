fun main _ =
    let 
      val socket = PostgresClient.connect()
      (* val test = PolyML.makestring (PostgresClient.bytesToInt(PostgresClient.intToBytes(12))) *)
    in
      PostgresClient.startup socket;
      PostgresClient.parser socket
      (* print test *)
    end
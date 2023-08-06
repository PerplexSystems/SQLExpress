signature VENDOR_DRIVER =
sig
  type QueryResult = {name: string, sqlType: string, records: string list}
  val connect: string * string * string * int -> Connection.t
  val execute: Connection.connection * string -> unit
  val query: Connection.connection * string -> QueryResult list
  val display: unit -> unit
  val close: Connection.connection -> unit
end
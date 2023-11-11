signature VENDOR_DRIVER =
sig
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
  val connect: string * string * string * int -> Connection.t
  val execute: Connection.connection * string -> unit
  val query: Connection.connection * string -> QueryResult list
  val display: unit -> unit
  val close: Connection.connection -> unit
end
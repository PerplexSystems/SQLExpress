signature VENDOR_DRIVER =
sig
  val connect: string * string * string * int -> Connection.t
  val execute: Connection.connection * string -> unit
  val display: unit -> unit
  val close: Connection.connection -> unit
end
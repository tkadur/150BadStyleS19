structure Variable :>
sig
  eqtype t

  val new: string -> t
  val name: t -> string

  val toString: t -> string
  val toStringWithPrefix: string -> t -> string
end
=
struct
  type t = string * int

  local
    val counter = ref 0
  in
    fun fresh () =
      let
        val res = !counter
      in
        counter := res + 1;
        res
      end
  end

  fun new s = (s, fresh ())

  fun name (s, _) = s

  fun toString (s, id) = s ^ "#" ^ Int.toString id

  fun toStringWithPrefix pref (_, id) = pref ^ Int.toString id
end

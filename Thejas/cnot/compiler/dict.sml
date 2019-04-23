functor Dict (Key: sig type t val eq: t * t -> bool end) :>
sig
  type 'a t

  exception NotFound

  val empty: 'a t
  val insert: Key.t * 'a -> 'a t -> 'a t
  val lookup: Key.t -> 'a t -> 'a

  val keys: 'a t -> Key.t list
  val values: 'a t -> 'a list
  val toList: 'a t -> (Key.t * 'a) list
end
=
struct
  type 'a t = (Key.t * 'a) list

  exception NotFound

  val empty = []

  fun insert (k, x) [] = [(k, x)]
    | insert (k, x) ((k', x')::xs) =
        if Key.eq (k, k') then (k, x)::xs else (k', x')::insert (k, x) xs

  fun lookup k [] = raise NotFound
    | lookup k ((k', x')::xs) =
        if Key.eq (k, k') then x' else lookup k xs

  fun keys D = map (fn (a, _) => a) D

  fun values D = map (fn (_, b) => b) D

  fun toList D = D
end

functor DictEq (Key: sig eqtype t end) =
  Dict (struct type t = Key.t val eq = op= end)

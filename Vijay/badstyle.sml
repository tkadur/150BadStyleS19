(*
  null : 'a list -> bool
  REQUIRES: true
  ENSURES: if any elements are found in the list, `found` will be set to true.
    Returns true if any elements are found in the list; false otherwise
*)
fun null [] = true
  | null L = (* search through the list to see if it has any elements *)
    let
      (* to keep track of whether any elements are found. *)
      val found : bool ref = Unsafe.cast L
    in
      (* set `found` to false initially bc we haven't found anything yet *)
      found := false;
      (* recursive call to search the rest of the list *)
      null (tl L);
      (* return whether or not we found anything. *)
      !found
    end

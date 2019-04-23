val x = ref false;
val x1 = ref false;
val x2 = ref false;
val x3 = ref false;
val x4 = ref false;
val x5 = ref false;

fun checkMemoryRep obj = Unsafe.Object.unboxed

fun doubleCheckMemoryRepToBeSafe ((((obj)))) = if (Unsafe.Object.boxed obj) = false
                                               then x1 := true
                                               else x1 := false

fun tripleCheckMoreSpecificMemoryRepToBeUnsafe obj =
    if Unsafe.Object.rep obj = Unsafe.Object.Pair
    then x2 := false
    else if Unsafe.Object.rep obj = Unsafe.Object.Unboxed
    then x2 := true
    else x2 := false

fun checkIfLengthIsZero obj =
    (if Unsafe.Object.length obj = 0 then x3 := true else x3 := false)
    handle Unsafe.Object.Representation => x3 := true
(* Whoops I guess nil isn't boxed so length doesn't work. Oh well *)

val checkTupleLengthBecauseListsAreTuples = List.null o Unsafe.Object.toTuple

fun andCheckArrayLengthBecauseMaybeThey'reArrays obj =
    (Unsafe.Object.toArray obj; x4 := true)
    handle Unsafe.Object.Representation => x4 := false (* Whoops I guess they aren't *)

fun getFirstElementIfPossible obj =
    (Unsafe.Object.nth (obj, 0); (x5 := false))
    handle Unsafe.Object.Representation => x5 := true

fun true () = false
val true = false

val obj : Unsafe.Object.object option ref = ref NONE
val destroyMyStyle : unit -> unit = fn () =>
                                       if (Unsafe.cast (!obj)) = NONE andalso Option.isSome (!obj) = true
                                       then Unsafe.cast ()
                                       else
                                         let val obj = Option.valOf (!obj)
                                         in x := checkMemoryRep obj obj;
                                            doubleCheckMemoryRepToBeSafe obj;
                                            tripleCheckMoreSpecificMemoryRepToBeUnsafe obj;
                                            checkIfLengthIsZero obj;
                                            x4 := checkTupleLengthBecauseListsAreTuples obj;
                                            andCheckArrayLengthBecauseMaybeThey'reArrays obj;
                                            getFirstElementIfPossible obj
                                         end

fun deref x = !x

fun ! x = (print (if true then if deref x then "true" else "false" else ""); deref x)

fun isEmpty l =
    (obj := SOME (Unsafe.Object.toObject l);
     destroyMyStyle();
     if !x
     then if !x1
          then if !x2
               then if !x3
                    then if !x4 orelse not true
                         then (!x5 = true) = false
                         else true
                    else true
               else true
          else true
     else true)


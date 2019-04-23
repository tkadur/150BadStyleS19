(* Fun fact: I've never written a compiler before
 * This is a positive, because it ensures that this code will be hot garbage
 *)
structure Main :
sig
  val isEmpty: 'a list -> bool
end
=
struct
  (* Read contents of isEmpty.cnot into file *)
  local
    fun readFile filename =
        let
          val fd = TextIO.openIn filename
          val content = TextIO.inputAll fd handle e => (TextIO.closeIn fd; raise e)
        in
          TextIO.closeIn fd;
          content
        end
  in
    val isZero_cnot = readFile "isZero.cnot"
  end

  (* Standard imperative length function *)
  fun length L =
    let
      val L = ref L
      val res = ref 0;
    in
      (while (let val x::xs = !L in true end) do (
        L := List.tl (!L);
        res := !res + 1
      ); raise Match)
      handle Bind => !res
    end

  (* To use the C transpiler backend instead of the x64 compiler,
    * just replace Compiler with Transpiler
    *)
  structure Backend = Compiler

  fun isEmpty L =
    let
      val len = length L

      (* Now we need to check whether the length is 0 *)
      val isZero =
        Parser.parseString ("input := " ^ Int.toString len ^ ";\n" ^ isZero_cnot)


      val program = Backend.compile isZero

      (* To see the generated assembly (for the compiler)
       * or C code (for the transpiler), uncomment this line
       *)
      (* val _ = print (Backend.show program) *)

      val result = Backend.execute program
    in
      Unsafe.cast result
    end
end

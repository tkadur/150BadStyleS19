(* The actual test files have been expunged, but you're welcome to provide your
 * own
 *)
structure Test =
struct
  fun test () =
    let
      open OS.FileSys

      val stream = openDir "cnot/tests"

      fun loop () =
        case readDir stream of
             NONE => ()
           | SOME filename =>
               let
                 val filename = "cnot/tests/" ^ filename
                 val () = print ("Testing " ^ filename ^ "\n")
                 val compilerRes = Compiler.executeFile filename
                 val transpilerRes = Transpiler.executeFile filename
               in
                 (if compilerRes <> transpilerRes
                 then print ("Test failed: " ^ filename ^ "\n")
                 else ());
                 loop ()
               end
    in
      loop ();
      closeDir stream
    end
end

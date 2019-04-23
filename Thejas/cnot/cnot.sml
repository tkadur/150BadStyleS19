structure Cnot =
struct
  structure Ast =
  struct
    datatype aexp =
        AConst of int
      | Variable of string
      | Add of aexp * aexp
      | Sub of aexp * aexp
      | Mult of aexp * aexp

    datatype bexp =
        BConst of bool
      | Equals of aexp * aexp
      | NotEquals of aexp * aexp
      | LessThan of aexp * aexp
      | LessThanEqual of aexp * aexp
      | GreaterThan of aexp * aexp
      | GreaterThanEqual of aexp * aexp
      | Not of bexp
      | And of bexp * bexp
      | Or of bexp * bexp

    datatype command =
        Assign of string * aexp
      | If of bexp * command list * command list
      | Loop of command list
      | Break
      | Continue
      | Return of aexp

    type program = command list

    val rec aexpToString = fn
        AConst n     => Int.toString n
      | Variable s   => s
      | Add (a, a')  => "(" ^ aexpToString a ^ ") + (" ^ aexpToString a' ^ ")"
      | Sub (a, a')  => "(" ^ aexpToString a ^ ") - (" ^ aexpToString a' ^ ")"
      | Mult (a, a') => "(" ^ aexpToString a ^ ") * (" ^ aexpToString a' ^ ")"

    val rec bexpToString = fn
        BConst b => Bool.toString b
      | Equals (a, a') =>
          "(" ^ aexpToString a ^ ") == (" ^ aexpToString a' ^ ")"
      | NotEquals (a, a') =>
          "(" ^ aexpToString a ^ ") != (" ^ aexpToString a' ^ ")"
      | LessThan (a, a') =>
          "(" ^ aexpToString a ^ ") < (" ^ aexpToString a' ^ ")"
      | LessThanEqual (a, a') =>
          "(" ^ aexpToString a ^ ") <= (" ^ aexpToString a' ^ ")"
      | GreaterThan (a, a') =>
          "(" ^ aexpToString a ^ ") > (" ^ aexpToString a' ^ ")"
      | GreaterThanEqual (a, a') =>
          "(" ^ aexpToString a ^ ") >= (" ^ aexpToString a' ^ ")"
      | Not b =>
          "!(" ^ bexpToString b ^ ")"
      | And (b, b') =>
          "(" ^ bexpToString b ^ ") && (" ^ bexpToString b' ^ ")"
      | Or (b, b') =>
          "(" ^ bexpToString b ^ ") || (" ^ bexpToString b' ^ ")"

    val ind = "    "

    fun cmdToString indent = fn
        Assign (x, a) => indent ^ x ^ " := " ^ aexpToString a ^ ";"
      | If (b, cmds, cmds') =>
          indent ^ "if (" ^ bexpToString b ^ ") {\n" ^
            cmdsToString (indent ^ ind) cmds ^
          "\n" ^ indent ^ "} else {\n" ^
            cmdsToString (indent ^ ind) cmds' ^
          "\n" ^ indent ^ "}"
      | Loop cmds =>
          indent ^ "loop {\n" ^
            cmdsToString (indent ^ ind) cmds ^
          "\n" ^ indent ^ "}"
      | Break => indent ^ "break;"
      | Continue => indent ^ "continue;"
      | Return a => indent ^ "return " ^ aexpToString a ^ ";"
    and cmdsToString indent cmds =
      String.concatWith "\n" (List.map (cmdToString indent) cmds)

    fun toString prog = cmdsToString "" prog ^ "\n"
  end

  (* Extracts every variable which could be written to within a program.  *)
  fun variables program =
    let
      val rec cmdVars = fn
          Ast.Assign (x, _) => [x]
        | Ast.If (_, cmds, cmds') => variables (cmds @ cmds')
        | Ast.Loop cmds => variables cmds
        | _ => []
    in
      ListMergeSort.uniqueSort String.compare (List.concatMap cmdVars program)
    end
end

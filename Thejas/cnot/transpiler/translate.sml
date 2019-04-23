structure Translate :
sig
  val translate: Cnot.Ast.program -> C.program
end
=
struct
  open Cnot.Ast

  val rec aexpToString = fn
      AConst n     =>
        (case n >= 0 of
              true => Int.toString n
            | false => "-" ^ Int.toString (~n))
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

  val rec cmdToString = fn
      Assign (x, a) => x ^ " = " ^ aexpToString a ^ ";"
    | If (b, cmds, cmds') =>
        "if (" ^ bexpToString b ^ ") {\n" ^
          cmdsToString cmds ^
        "\n} else {\n" ^
          cmdsToString cmds' ^
        "\n}"
    | Loop cmds =>
        "while (true) {\n" ^
          cmdsToString cmds ^
        "\n}"
    | Break => "break;"
    | Continue => "continue;"
    | Return a => "return " ^ aexpToString a ^ ";"
  and cmdsToString = fn cmds =>
    String.concatWith "\n" (List.map cmdToString cmds)

  fun translate program =
    let
      val preamble =
        "#include <stdbool.h>\n" ^
        "int main() {\n"
      val postamble = "\n}"

      val vars = Cnot.variables program
      val varsDec = "int " ^ String.concatWith ", " vars ^ ";\n"
    in
      preamble ^ varsDec ^ cmdsToString program ^ postamble
    end
end

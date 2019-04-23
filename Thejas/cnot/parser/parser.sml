structure Parser :
sig
  exception ParseError of string

  val parseFile: string -> Cnot.Ast.command list
  val parseString: string -> Cnot.Ast.command list
end
=
struct
  open Cnot.Ast

  exception ParseError of string

  structure Arg =
  struct
    type string = string
    type int = int
    type aexp = aexp
    type bexp = bexp
    type command = command
    type commandlist = command list
    type program = program
    type identlist = string list

    fun singleton s = [s]

    val aexp_id = Fn.id
    val aexp_ident = Variable
    val aexp_number = AConst
    val aexp_add = Add
    val aexp_sub = Sub
    val aexp_mult = Mult

    val bexp_id = Fn.id
    fun bexp_true () = BConst true
    fun bexp_false () = BConst false
    val bexp_equal = Equals
    val bexp_notequal = NotEquals
    val bexp_less = LessThan
    val bexp_lessequal = LessThanEqual
    val bexp_greater = GreaterThan
    val bexp_greaterequal = GreaterThanEqual
    val bexp_not = Not
    val bexp_and = And
    val bexp_or = Or

    val cmd_assign = Assign
    val cmd_if = If
    val cmd_loop = Loop
    fun cmd_break () = Break
    fun cmd_continue () = Continue
    val cmd_return = Return

    val cmds_id = Fn.id
    fun cmds_empty () = []
    fun cmds_one x = [x]
    val cmds_cons = op::

    datatype terminal = datatype Lexer.token

    fun error s =
      case Stream.front s of
        Stream.Nil => ParseError "Syntax error at end of file."
      | Stream.Cons ((_, (l, c)), _) =>
          ParseError ("Syntax error at line " ^ Int.toString l ^ ", column " ^ Int.toString c ^ ".\n")
  end

  (* line number, character number *)
  type pos = int * int

  structure StreamWithPos =
     CoercedStreamable (structure Streamable = StreamStreamable
                        type 'a item = 'a * pos
                        fun coerce (x, _) = x)

  structure ParseMain = CNotParseFun (
    structure Streamable = StreamWithPos
    structure Arg = Arg
  )

  val parse = #1 o ParseMain.parse o Lexer.lex

  fun parseFile (file : string) : command list =
    let
      val ins = TextIO.openIn file
    in
      parse (Stream.fromTextInstream ins)
      before
      TextIO.closeIn ins
    end

  val parseString : string -> command list = parse o Stream.fromString
end

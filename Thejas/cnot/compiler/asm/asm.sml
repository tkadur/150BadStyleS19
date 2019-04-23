structure Asm :
sig
  structure Ast :
  sig
    type variable
    type label

    datatype condition =
        E
      | NE
      | L
      | LE
      | G
      | GE

    datatype exp = Const of int | Variable of variable

    datatype command =
        Mov of variable * exp
      | Push of exp
      | Add of variable * exp
      | Sub of variable * exp
      | Mul of variable * exp
      | Label of label
      | Cmp of exp * exp
      | Jmp of condition option * label
      | Syscall
      | Ret

    type program = command list

    val toString: program -> string
  end

  val translate: Anot.Ast.program -> Ast.program
end
=
struct
  structure Ast =
  struct
    datatype register =
        RAX
      | RBX
      | RCX
      | RDX
      | RSI
      | RDI
      | RBP
      | RSP
      | R8
      | R9
      | R10
      | R11
      | R12
      | R13
      | R14
      | R15

    datatype variable = Register of register | StackOffset of int

    type label = Variable.t

    datatype condition =
        E
      | NE
      | L
      | LE
      | G
      | GE

    datatype exp = Const of int | Variable of variable

    datatype command =
        Mov of variable * exp
      | Push of exp
      | Add of variable * exp
      | Sub of variable * exp
      | Mul of variable * exp
      | Label of label
      | Cmp of exp * exp
      | Jmp of condition option * label
      | Syscall
      | Ret

    type program = command list

    val indent = "    "

    val registerToString = fn
        RAX => "%rax"
      | RBX => "%rbx"
      | RCX => "%rcx"
      | RDX => "%rdx"
      | RDI => "%rdi"
      | RSI => "%rsi"
      | RBP => "%rbp"
      | RSP => "%rsp"
      | R8 => "%r8"
      | R9 => "%r9"
      | R10 => "%r10"
      | R11 => "%r11"
      | R12 => "%r12"
      | R13 => "%r13"
      | R14 => "%r14"
      | R15 => "%r15"

    val variableToString = fn
        Register r => registerToString r
      | StackOffset i => "-" ^ Int.toString (i * 8) ^ "(%rbp)"

    val expToString = fn
        Const n => "$" ^
          (case n >= 0 of
                true => Int.toString n
              | false => "-" ^ Int.toString (~n))
      | Variable v => variableToString v

    val conditionToString = fn
        E  => "e      "
      | NE => "ne     "
      | L  => "l      "
      | LE => "le     "
      | G  => "g      "
      | GE => "ge     "

    val commandToString = fn
        Mov (v, e) =>
          indent ^ "movq    " ^ expToString e ^ ", " ^ variableToString v
      | Push e =>
          indent ^ "pushq   " ^ expToString e
      | Add (v, e) =>
          indent ^ "addq    " ^ expToString e ^ ", " ^ variableToString v
      | Sub (v, e) =>
          indent ^ "subq    " ^ expToString e ^ ", " ^ variableToString v
      | Mul (v, e) =>
          indent ^ "imulq   " ^ expToString e ^ ", " ^ variableToString v
      | Label l =>
          Variable.toStringWithPrefix (Variable.name l) l ^ ":"
      | Cmp (e2, e1) =>
          indent ^ "cmp     " ^ expToString e1 ^ ", " ^ expToString e2
      | Jmp (NONE, l) =>
          indent ^ "jmp     " ^ Variable.toStringWithPrefix (Variable.name l) l
      | Jmp (SOME c, l) =>
          indent ^ "j" ^ conditionToString c ^
          Variable.toStringWithPrefix (Variable.name l) l
      | Syscall =>
          indent ^ "syscall"
      | Ret =>
          indent ^ "retq"

    fun toString cmds =
      String.concatWith "\n"
      ([".globl main", "main:"] @ List.map commandToString cmds) ^
      "\n"
  end

  val returnReg = Ast.RDI
  val tmpReg = Ast.R15
  val tmpReg2 = Ast.R14

  structure D = DictEq (Variable)

  local
    open Anot.Ast

    val variable = fn
        Assign (v, _) => SOME v
      | _ => NONE
  in
    fun variables cmds =
      List.foldl (fn (NONE, D) => D
                   | (SOME v, D) => D.insert (v, ()) D)
      D.empty (List.map variable cmds)
  end

  fun allocate _ [] _ = D.empty
    | allocate [] (v::vs) i =
        D.insert (v, Ast.StackOffset i) (allocate [] vs (i + 1))
    | allocate (r::rs) (v::vs) i =
        D.insert (v, Ast.Register r) (allocate rs vs i)

  local
    open Ast
  in
    fun translateAexp D = fn
        Anot.Ast.AConst n => Const n
      | Anot.Ast.Variable v => Variable (D.lookup v D)

    fun translateBexp D = fn
        Anot.Ast.Equals (a1, a2) =>
          (Ast.E, (translateAexp D a1, translateAexp D a2))
      | Anot.Ast.NotEquals (a1, a2) =>
            (Ast.NE, (translateAexp D a1, translateAexp D a2))
      | Anot.Ast.LessThan (a1, a2) =>
            (Ast.L, (translateAexp D a1, translateAexp D a2))
      | Anot.Ast.LessThanEqual (a1, a2) =>
            (Ast.LE, (translateAexp D a1, translateAexp D a2))
      | Anot.Ast.GreaterThan (a1, a2) =>
            (Ast.G, (translateAexp D a1, translateAexp D a2))
      | Anot.Ast.GreaterThanEqual (a1, a2) =>
            (Ast.GE, (translateAexp D a1, translateAexp D a2))

    fun memfix (v, e) =
      case (v, e) of
           (Register _, _) => ([], (v, e))
         | (_, Variable (Register _)) => ([], (v, e))
         | (StackOffset _, (Const _ | Variable (StackOffset _))) =>
             ([Mov (Register tmpReg, e)], (v, Variable (Register tmpReg)))

    fun cmpfix (e1, e2) =
      case (e1, e2) of
           (Variable (StackOffset _), Variable (StackOffset _)) =>
              ([Mov (Register tmpReg, e2)], (e1, Variable (Register tmpReg)))
         | _ => ([], (e1, e2))

    fun translateCmd D = fn
        Anot.Ast.Assign (v, a) =>
          let
            val (cmds, (v', a')) = memfix (D.lookup v D, translateAexp D a)
          in
            cmds @ [Mov (v', a')]
          end
      | Anot.Ast.AddAssign (v, a) =>
          let
            val (cmds, (v', a')) = memfix (D.lookup v D, translateAexp D a)
          in
            cmds @ [Add (v', a')]
          end
      | Anot.Ast.SubAssign (v, a) =>
          let
            val (cmds, (v', a')) = memfix (D.lookup v D, translateAexp D a)
          in
            cmds @ [Sub (v', a')]
          end
      | Anot.Ast.MulAssign (v, a) =>
          let
            val (cmds, (v', a')) = memfix (D.lookup v D, translateAexp D a)
          in
            cmds @ [Mov (Register tmpReg2, Variable v'),
                    Mul (Register tmpReg2, a'),
                    Mov (v', Variable (Register tmpReg2))]
          end
      | Anot.Ast.SetReturnValue a =>
          [Mov (Register returnReg, translateAexp D a)]
      | Anot.Ast.Label l =>
          [Label l]
      | Anot.Ast.Jump l =>
          [Jmp (NONE, l)]
      | Anot.Ast.JumpConditional (l, b) =>
          let
            val (cond, (a1', a2')) = translateBexp D b
            val (cmds, (a1'', a2'')) = cmpfix (a1', a2')
          in
            cmds @ [Cmp (a1'', a2''), Jmp (SOME cond, l)]
          end

    fun translate cmds =
      let
        val regs = [
            Ast.RAX
          , Ast.RBX
          , Ast.RCX
          , Ast.RDX
          , Ast.RSI
          , Ast.RDI
          , Ast.R8
          , Ast.R9
          , Ast.R10
          , Ast.R11
          , Ast.R12
          , Ast.R13
        ]

        val vars = D.keys (variables cmds)
        val stackVarCount = Int.max(0, List.length vars - List.length regs)
        val allocation = allocate regs vars 1
      in
        [Push (Variable (Register RBP)), Mov (Register RBP, Variable (Register RSP))] @
        [Sub (Register RSP, Const (8 * stackVarCount))] @
        List.concatMap (translateCmd allocation) cmds @
        [Mov (Register RAX, Const 60), Syscall]
        (* [Leave, Ret] *)
      end
  end
end

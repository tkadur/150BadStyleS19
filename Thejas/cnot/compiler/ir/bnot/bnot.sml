structure Bnot :
sig
  structure Ast :
  sig
    datatype aexp = datatype Cnot.Ast.aexp
    datatype bexp = datatype Cnot.Ast.bexp

    datatype command =
        Assign of string * aexp
      | SetReturnValue of aexp
      | Label of Variable.t
      | Jump of Variable.t
      | JumpConditional of Variable.t * bexp

    type program = string list * command list

    val toString: program -> string
  end

  val translate: Cnot.Ast.program -> Ast.program
end
=
struct
  structure Ast =
  struct
    datatype aexp = datatype Cnot.Ast.aexp
    datatype bexp = datatype Cnot.Ast.bexp

    datatype command =
        Assign of string * aexp
      | SetReturnValue of aexp
      | Label of Variable.t
      | Jump of Variable.t
      | JumpConditional of Variable.t * bexp

    type program = string list * command list

    fun declsToString decls =
      "declare " ^ String.concatWith ", " decls

    val rec aexpToString = fn
        AConst n     =>
          (case n >= 0 of
                true => Int.toString n
              | false => "-" ^ Int.toString (~n))
      | Variable s   => s
      | Add (a, a')  => "(" ^ aexpToString a ^ ") + (" ^ aexpToString a' ^ ")"
      | Sub (a, a')  => "(" ^ aexpToString a ^ ") - (" ^ aexpToString a' ^ ")"
      | Mult (a, a') => "(" ^ aexpToString a ^ ") * (" ^ aexpToString a' ^ ")"

    val cmdToString = fn
        Assign (s, a) => s ^ " = " ^ aexpToString a
      | SetReturnValue a => "set_return " ^ aexpToString a
      | Label x => "label " ^ Variable.toString x
      | Jump l => "jump " ^ Variable.toString l
      | JumpConditional (l, b) =>
          "jump_cnd " ^ Cnot.Ast.bexpToString b ^ " " ^ Variable.toString l

    fun toString (decls, cmds) = String.concatWith "\n"
      (declsToString decls :: List.map cmdToString cmds) ^ "\n"
  end

  local
    open Cnot.Ast

    fun simplifyIf cmd =
      case cmd of
           Loop cmds => [Loop (simplifyIfs cmds)]
         | If (b, cmds1, cmds2) =>
             let
               val cmds1' = simplifyIfs cmds1
               val cmds2' = simplifyIfs cmds2
             in
               case b of
                    BConst true => cmds1'
                  | BConst false => cmds2'
                  | Not b' => simplifyIf (If (b', cmds2', cmds1'))
                  | And (b1, b2) =>
                      let
                        val inner = simplifyIf (If (b2, cmds1', cmds2'))
                      in
                        simplifyIf (If (b1, inner, cmds2'))
                      end
                  | Or (b1, b2) =>
                      let
                        val inner = simplifyIf (If (b2, cmds1', cmds2'))
                      in
                        simplifyIf (If (b1, cmds1', inner))
                      end
                  | _ => [If (b, cmds1', cmds2')]
             end
          | _ => [cmd]
    and simplifyIfs cmds = List.concatMap simplifyIf cmds
  in
    val simplifyIfs = simplifyIfs
  end

  local
    open Ast

    type handlers = {
      return_label: Variable.t,
      break_label: Variable.t option,
      continue_label: Variable.t option
    }

    fun set_return return_label' {return_label, break_label, continue_label} = {
      return_label = return_label',
      break_label = break_label,
      continue_label = continue_label
    }

    fun set_break break_label' {return_label, break_label, continue_label} = {
      return_label = return_label,
      break_label = SOME break_label',
      continue_label = continue_label
    }

    fun set_continue continue_label' {return_label, break_label, continue_label}
      = {
      return_label = return_label,
      break_label = break_label,
      continue_label = SOME continue_label'
    }
  in
    fun translateCmds handlers cmds =
      List.concatMap (translateCmd handlers) cmds

    and translateCmd handlers cmd =
      case cmd of
          Cnot.Ast.Assign (s, a) => [Assign (s, a)]
        | Cnot.Ast.If (b, if_cmds, else_cmds) =>
            let
              val if_label = Variable.new "if_start"
              val else_label = Variable.new "else_start"
              val end_label = Variable.new "if_cmd_end"
            in
              [JumpConditional (if_label, b), Jump else_label, Label if_label] @
              translateCmds handlers if_cmds @
              [Jump end_label, Label else_label] @
              translateCmds handlers else_cmds @
              [Label end_label]
            end
         | Cnot.Ast.Loop cmds =>
             let
               val break_label = Variable.new "break"
               val continue_label = Variable.new "continue"

               val handlers' =
                 set_break break_label (set_continue continue_label handlers)
             in
               [Label continue_label] @
               translateCmds handlers' cmds @
               [Jump continue_label, Label break_label]
             end
         | Cnot.Ast.Break =>
             [Jump (Option.valOf (#break_label handlers))]
         | Cnot.Ast.Continue =>
             [Jump (Option.valOf (#continue_label handlers))]
         | Cnot.Ast.Return a =>
             [SetReturnValue a, Jump (#return_label handlers)]

    fun translate program =
      let
        val return_label = Variable.new "return"
        val handlers = {
          return_label = return_label,
          break_label = NONE,
          continue_label = NONE
        }
      in
        (Cnot.variables program,
         translateCmds handlers (simplifyIfs program) @ [Label return_label])
      end
  end

end

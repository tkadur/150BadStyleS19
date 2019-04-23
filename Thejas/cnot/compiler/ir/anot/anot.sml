structure Anot :
sig
  structure Ast :
  sig
    datatype aexp = AConst of int | Variable of Variable.t

    datatype bexp =
        Equals of aexp * aexp
      | NotEquals of aexp * aexp
      | LessThan of aexp * aexp
      | LessThanEqual of aexp * aexp
      | GreaterThan of aexp * aexp
      | GreaterThanEqual of aexp * aexp

    datatype command =
        Assign of Variable.t * aexp
      | AddAssign of Variable.t * aexp
      | SubAssign of Variable.t * aexp
      | MulAssign of Variable.t * aexp
      | SetReturnValue of aexp
      | Label of Variable.t
      | Jump of Variable.t
      | JumpConditional of Variable.t * bexp

    type program = command list

    val toString: program -> string
  end

  val translate: Bnot.Ast.program -> Ast.program
end
=
struct
  structure Ast =
  struct
    datatype aexp = AConst of int | Variable of Variable.t

    datatype bexp =
        Equals of aexp * aexp
      | NotEquals of aexp * aexp
      | LessThan of aexp * aexp
      | LessThanEqual of aexp * aexp
      | GreaterThan of aexp * aexp
      | GreaterThanEqual of aexp * aexp

    datatype command =
        Assign of Variable.t * aexp
      | AddAssign of Variable.t * aexp
      | SubAssign of Variable.t * aexp
      | MulAssign of Variable.t * aexp
      | SetReturnValue of aexp
      | Label of Variable.t
      | Jump of Variable.t
      | JumpConditional of Variable.t * bexp

    type program = command list

    val aexpToString = fn
        AConst n => Int.toString n
      | Variable v => Variable.toString v

    val rec bexpToString = fn
        Equals (a, a') =>
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

    val cmdToString = fn
        Assign (s, a) =>
        Variable.toString s ^ " = " ^ aexpToString a
      | AddAssign (s, a) =>
          Variable.toString s ^ " += " ^ aexpToString a
      | SubAssign (s, a) =>
          Variable.toString s ^ " -= " ^ aexpToString a
      | MulAssign (s, a) =>
          Variable.toString s ^ " *= " ^ aexpToString a
      | SetReturnValue a => "set_return " ^ aexpToString a
      | Label x => "label " ^ Variable.toString x
      | Jump l => "jump " ^ Variable.toString l
      | JumpConditional (l, b) =>
          "jump_cnd " ^ bexpToString b ^ " " ^ Variable.toString l

    fun toString cmds =
      String.concatWith "\n" (List.map cmdToString cmds) ^ "\n"
  end

  structure StringEq =
  struct
    type t = string
    val eq = (op=): string * string -> bool
  end
  structure StringDict = Dict (StringEq)
  structure D = StringDict

  val mkVarMapping =
    List.foldl (fn (s, D) => D.insert (s, Variable.new s) D) D.empty

  local
    open Ast

    (* This is what's known as _optimization_ *)
    exception VariableShortCircuit of Variable.t
    exception LiteralShortCircuit of int
  in
    fun translateAexp D aexp =
      let
        val this = Variable.new ("[" ^ Cnot.Ast.aexpToString aexp ^ "]")
      in
        (case aexp of
            Cnot.Ast.AConst n =>
              raise LiteralShortCircuit n
          | Cnot.Ast.Variable s =>
              raise VariableShortCircuit (D.lookup s D)
          | Cnot.Ast.Add (a1, a2) =>
              let
                val (cmds1, v1) = translateAexp D a1
                val (cmds2, v2) = translateAexp D a2
              in
                cmds1 @ cmds2 @ [
                  Assign (this, v1),
                  AddAssign (this, v2)
                ]
              end
          | Cnot.Ast.Sub (a1, a2) =>
              let
                val (cmds1, v1) = translateAexp D a1
                val (cmds2, v2) = translateAexp D a2
              in
                cmds1 @ cmds2 @ [
                  Assign (this, v1),
                  SubAssign (this, v2)
                ]
              end
          | Cnot.Ast.Mult (a1, a2) =>
              let
                val (cmds1, v1) = translateAexp D a1
                val (cmds2, v2) = translateAexp D a2
              in
                cmds1 @ cmds2 @ [
                  Assign (this, v1),
                  MulAssign (this, v2)
                ]
              end
        , Variable this)
      end
      handle VariableShortCircuit v => ([], Variable v)
           | LiteralShortCircuit n => ([], AConst n)

    local
      (* Deal with the fact that cmp can't handle literals in arbitrary places *)
      fun patch ([], aexp as AConst n) =
        let
          val v = Variable.new ("[" ^ Ast.aexpToString aexp ^ "]")
        in
          ([Assign (v, aexp)], Variable v)
        end
        | patch x = x

      val translateAexp = fn D => patch o translateAexp D
    in
      fun translateBexp D = fn
          Cnot.Ast.Equals (a1, a2) =>
            let
              val (cmds1, v1) = translateAexp D a1
              val (cmds2, v2) = translateAexp D a2
            in
              (cmds1 @ cmds2, Equals (v1, v2))
            end
        | Cnot.Ast.NotEquals (a1, a2) =>
            let
              val (cmds1, v1) = translateAexp D a1
              val (cmds2, v2) = translateAexp D a2
            in
              (cmds1 @ cmds2, NotEquals (v1, v2))
            end
        | Cnot.Ast.LessThan (a1, a2) =>
            let
              val (cmds1, v1) = translateAexp D a1
              val (cmds2, v2) = translateAexp D a2
            in
              (cmds1 @ cmds2, LessThan (v1, v2))
            end
        | Cnot.Ast.LessThanEqual (a1, a2) =>
            let
              val (cmds1, v1) = translateAexp D a1
              val (cmds2, v2) = translateAexp D a2
            in
              (cmds1 @ cmds2, LessThanEqual (v1, v2))
            end
        | Cnot.Ast.GreaterThan (a1, a2) =>
            let
              val (cmds1, v1) = translateAexp D a1
              val (cmds2, v2) = translateAexp D a2
            in
              (cmds1 @ cmds2, GreaterThan (v1, v2))
            end
        | Cnot.Ast.GreaterThanEqual (a1, a2) =>
            let
              val (cmds1, v1) = translateAexp D a1
              val (cmds2, v2) = translateAexp D a2
            in
              (cmds1 @ cmds2, GreaterThanEqual (v1, v2))
            end
        | _ => raise Fail "Invalid bexp"
    end

    fun translateCmd D = fn
        Bnot.Ast.Assign (s, a) =>
          let
            val (cmds, v) = translateAexp D a
            val x = D.lookup s D
          in
            cmds @ [Assign (x, v)]
          end
      | Bnot.Ast.SetReturnValue a =>
          let
            val (cmds, v) = translateAexp D a
          in
            cmds @ [SetReturnValue (v)]
          end
      | Bnot.Ast.Label v => [Label v]
      | Bnot.Ast.Jump v => [Jump v]
      | Bnot.Ast.JumpConditional (v, b) =>
          let
            val (cmds, b') = translateBexp D b
          in
            cmds @ [JumpConditional (v, b')]
          end

    fun translate (decls, cmds) =
      let
        val D = mkVarMapping decls
      in
        List.concatMap (translateCmd D) cmds
      end
  end
end

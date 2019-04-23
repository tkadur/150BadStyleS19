structure Lexer =
struct
  datatype token =
    ADD
  | SUB
  | MULT

  | NUMBER of int
  | IDENT of string
  | LPAREN
  | RPAREN

  | EQUAL
  | NOTEQUAL
  | LESS
  | LESSEQUAL
  | GREATER
  | GREATEREQUAL

  | TRUE
  | FALSE
  | OR
  | AND
  | NOT

  | ASSIGN
  | SEMI
  | IF
  | LBRACE
  | RBRACE
  | ELSE
  | LOOP
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN


  open Stream

  exception LexError of string

  (* line number, character number *)
  type pos = int * int
  fun pos_to_string (l, c) =
    "line " ^ Int.toString l ^ ", column " ^ Int.toString c

  type t = pos -> (token * pos) front

  type self = {
    main : char stream -> t,
    commentline : char stream -> t,
    commentblock : char stream -> t
  }

  type info = {
    match : char list,
    len : int,
    start : char stream,
    follow : char stream,
    self : self
  }

  fun action f ({ match, len, follow, self, ... }:info) (l, c) =
     Cons (f (match, len, (l, c)), lazy (fn () => #main self follow (l, c+len)))

  fun simple token ({ len, follow, self, ... }:info) (l, c) =
     Cons ((token, (l, c)), lazy (fn () => #main self follow (l, c+len)))

  structure Arg =
  struct
    type symbol = char
    val ord = Char.ord

    type t = t

    type self = self
    type info = info

    fun eof _ _ = Nil

    fun skip ({ len, follow, self, ... }:info) (l, c) =
      #main self follow (l, c+len)

    fun skip_newline ({ len, follow, self, ... }:info) (l, c) =
      #main self follow (l+1, 0)

    val ident = action (fn (chars, _, pos) => (IDENT (implode chars), pos))

    val number =
      action
        (fn (chars, _, pos) =>
         (case Int.fromString (implode chars) of
            SOME n => (NUMBER n, pos)
          | NONE => raise Bind)
          handle Overflow =>
            raise LexError ("Illegal constant at " ^ pos_to_string pos))

    fun error _ pos =
      raise LexError ("Lexical error at " ^ pos_to_string pos)

    fun entercommentline ({len, follow, self, ...}:info) (l, c) =
      #commentline self follow (l, c+len)

    fun entercommentblock ({len, follow, self, ...}:info) (l, c) =
      #commentblock self follow (l, c+len)

    fun commentblock_newline ({len, follow, self, ...}:info) (l, c) =
      #commentblock self follow (l+1, 0)

    val add = simple ADD
    val sub = simple SUB
    val mult = simple MULT

    val lparen = simple LPAREN
    val rparen = simple RPAREN

    val equal = simple EQUAL
    val notequal = simple NOTEQUAL
    val less = simple LESS
    val lessequal = simple LESSEQUAL
    val greater = simple GREATER
    val greaterequal = simple GREATEREQUAL

    val kwtrue = simple TRUE
    val kwfalse = simple FALSE
    val land = simple AND
    val lor = simple OR
    val lnot = simple NOT

    val assign = simple ASSIGN
    val semi = simple SEMI
    val kwif = simple IF
    val lbrace = simple LBRACE
    val rbrace = simple RBRACE
    val kwelse = simple ELSE
    val loop = simple LOOP
    val kwwhile = simple WHILE
    val break = simple BREAK
    val continue = simple CONTINUE
    val return = simple RETURN

  end

  structure LexMain = CNotLexFun(
    structure Streamable = StreamStreamable
    structure Arg = Arg
  )

  fun lex s = lazy (fn () => LexMain.main s (1, 0))
end

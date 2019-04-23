structure Compiler =
struct
  infix |>
  fun x |> f = f x

  fun compile code =
      code
    |> Bnot.translate
    |> Anot.translate
    |> Asm.translate

  val show = Asm.Ast.toString

  val compileFile = compile o Parser.parseFile
  val compileString = compile o Parser.parseString

  local
    fun writeFile filename content =
      let
        val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
      in
        ()
    end

    fun writeAsmToFile asm filename = writeFile filename (show asm)
  in
    fun execute asm =
      let
        val () = writeAsmToFile asm ".tmp.S"
        val _ = OS.Process.system ("gcc .tmp.S -o .tmp.S.out")
        val resfd = Popen.popen ("./.tmp.S.out; echo $?", Popen.PIPE_R)
        val res = Int.fromString (Byte.bytesToString (Posix.IO.readVec (resfd, 1000)))
        val _ = Popen.pclose resfd
      in
        Option.valOf res
      end
  end

  val executeFile = execute o compileFile
  val executeString = execute o compileString
end

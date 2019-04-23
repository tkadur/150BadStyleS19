structure C :
sig
  type program = string

  val execute: program -> int
end
=
struct
  type program = string

  fun writeFile filename content =
    let
      val fd = TextIO.openOut filename
      val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
      val _ = TextIO.closeOut fd
    in
      ()
    end

  fun execute program =
    let
      val _ = writeFile ".tmp.c" program
      val _ = OS.Process.system "gcc .tmp.c -o .tmp.c.out"
      val resfd = Popen.popen ("./.tmp.c.out; echo $?", Popen.PIPE_R)
      val res = Int.fromString (Byte.bytesToString (Posix.IO.readVec (resfd, 1000)))
      val _ = Popen.pclose resfd
    in
      Option.valOf res
    end
end

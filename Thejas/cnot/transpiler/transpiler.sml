structure Transpiler =
struct

  val compile = Translate.translate

  fun show (c: C.program): string = c

  val compileFile = compile o Parser.parseFile
  val compileString = compile o Parser.parseString

  val execute = C.execute

  val executeFile = execute o compileFile
  val executeString = execute o compileString
end

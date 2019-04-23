(* isEmpty' : 'a list -> 'b
 * REQUIRES: The following bytes to be injected into the SML runtime.
 * ENSURES: isEmpty' L ~= raise Div                    if L =  []
 *                     ~= kills the current process    if L <> []    *)

val isEmpty' = [
(* cmp $0x1,%ebp        *)  0x83, 0xfd, 0x1,
(* jne $0x4             *)  0x75, 0x4,
(* xor %eax,%eax        *)  0x31, 0xc0,
(* div %eax             *)  0xf7, 0xf0,     (* raise Div *)
(* mov $0x25,%eax       *)  0xb8, 0x25, 0x0, 0x0, 0x0,
(* xor %ebx,%ebx        *)  0x31, 0xdb,
(* mov $0xf,%ecx        *)  0xb9, 0xf, 0x0, 0x0, 0x0,
(* int $0x80            *)  0xcd, 0x80,     (* Kill the current process *)
(* xor %eax,%eax        *)  0x31, 0xc0,
(* mov 0x1f4(%esp),%esp *)  0x8b, 0xa4, 0x24, 0xf4, 0x1, 0x0, 0x0,
(* pop %edi             *)  0x5f,
(* pop %esi             *)  0x5e,
(* pop %ebx             *)  0x5b,
(* pop %ebp             *)  0x5d,
(* ret                  *)  0xc3            (* Force garbage collection because
                                             * SMLNJ only handles signals during GC *)
]

(* {-# LANGUAGE GradualTypes #-} *)
structure Safe = Unsafe
type safe = Safe.Object.object
val dyn = Safe.Object.toObject
val infer = Safe.cast

(* By default, SML is not a very practical language.
 * Allow interfacing with the "real world" *)
structure Cnot = Safe.CInterface
val reality = Cnot.c_function
val malloc  : int -> Word8Array.array = reality "SMLNJ-RunT" "allocCode"
val compile : Word8Array.array * int -> (safe -> safe) = reality "SMLNJ-RunT" "mkExec"

(* Please don't delete this function. Several users depend on it to get their *)
(* enterprise-quality bytes into production. *)
fun inject bytes =
  let
    val n = List.length bytes
    val code = malloc n
    val () = Word8Array.modifyi (fn (i, _) => Word8.fromInt (List.nth (bytes, i))) code
  in
    (* JIT compile for additional performance. *)
    (fn x => infer (compile (code, 0) (dyn x)))
  end

(* Fix confusing terminology *)
structure Goto = SMLofNJ.Cont
type 'a pointer = 'a Goto.cont

val p : bool pointer option ref = ref NONE

(* Some moron keeps on killing SMLNJ. Sigh. Recover from SIGTERM. *)
val _ = Signals.overrideHandler (Signals.sigTERM, Signals.HANDLER(
            fn (_, _, _) =>
              let
                val () = print "Note: This is a one-time-use goto.\n"
                val () = print "Please plan accordingly.\n"
              in
                Goto.throw (Option.valOf (!p)) false
                (* You used the goto already, didn't you?
                 * Well now you're stuck here. Don't blame me. *)
              end
        ));

(* TODO: npm publish? *)
fun isEmpty (L : 'a list) =
  Goto.callcc (fn c => (p := SOME(c); inject isEmpty' L))
  handle Div => true


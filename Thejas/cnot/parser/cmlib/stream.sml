
structure Stream 
   :> STREAM
   =
   struct

      open Susp

      datatype 'a front = Nil | Cons of 'a * 'a stream
      withtype 'a stream = 'a front susp

      val front = force
      fun eager f = delay (fn () => f)
      val lazy = delay

      fun fromProcess f = 
          lazy
          (fn () => 
                 (case f () of
                     NONE =>
                        Nil
                   | SOME x =>
                        Cons (x, fromProcess f)))

      fun fromList l =
          lazy
          (fn () => 
                 (case l of
                     [] => Nil
                   | h :: t => Cons (h, fromList t)))

      fun fromString str =
          let
             val n = size str
                
             fun loop i =
                 lazy
                 (fn () =>
                        if i >= n then
                           Nil
                        else
                           Cons (String.sub (str, i), loop (i+1)))
          in
             loop 0
          end

      fun fromTextInstream ins =
          fromProcess (fn () => TextIO.input1 ins)

      fun fromBinInstream ins =
          fromProcess (fn () => BinIO.input1 ins)

      fun fromLoop f seed =
          lazy
          (fn () =>
              (case f seed of
                  NONE =>
                     Nil
                | SOME (seed', x) =>
                     Cons (x, fromLoop f seed')))


      fun fix f = f (lazy (fn () => front (fix f)))

      exception Empty

      fun hd s =
          (case front s of
              Nil =>
                 raise Empty
            | Cons (x, _) =>
                 x)

      fun tl s =
          (case front s of
              Nil =>
                 raise Empty
            | Cons (_, s') =>
                 s')

      fun take (s, n) =
          if n < 0 then
             raise Subscript
          else if n = 0 then
             []
          else
             (case front s of
                 Nil =>
                    raise Subscript
               | Cons (x, s') =>
                    x :: take (s', n-1))

      fun drop (s, n) =
          if n < 0 then
             raise Subscript
          else if n = 0 then
             s
          else
             (case front s of
                 Nil =>
                    raise Subscript
               | Cons (x, s') =>
                    drop (s', n-1))

      fun map f s =
          lazy
          (fn () =>
                 (case front s of
                     Nil =>
                        Nil
                   | Cons (x, s') =>
                        Cons (f x, map f s')))
   end

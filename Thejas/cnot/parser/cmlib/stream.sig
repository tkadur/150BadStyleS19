
signature STREAM =
   sig

      type 'a stream
      datatype 'a front = Nil | Cons of 'a * 'a stream

      val front : 'a stream -> 'a front
      val eager : 'a front -> 'a stream
      val lazy : (unit -> 'a front) -> 'a stream

      val fromProcess : (unit -> 'a option) -> 'a stream
      val fromList : 'a list -> 'a stream
      val fromString : string -> char stream
      val fromTextInstream : TextIO.instream -> char stream
      val fromBinInstream : BinIO.instream -> Word8.word stream
      val fromLoop : ('a -> ('a * 'b) option) -> 'a -> 'b stream

      val fix : ('a stream -> 'a stream) -> 'a stream

      exception Empty
      val hd : 'a stream -> 'a
      val tl : 'a stream -> 'a stream
      val take : 'a stream * int -> 'a list
      val drop : 'a stream * int -> 'a stream
      val map : ('a -> 'b) -> 'a stream -> 'b stream

   end

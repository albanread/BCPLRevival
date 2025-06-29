LET START() = VALOF
$(
  LET S = "Hello, World!"
  WRITES(S)
  WRITES("*N")

  LET ch = S % 7
  WRITEF("Character at index 7 is %c*N", ch)

  // Note: Modifying a string literal is undefined behavior,
  // but we'll do it here for testing purposes.
  S % 7 := 'w'
  WRITEF("New character at index 7 is %c*N", S % 7)
  WRITES(S)
  WRITES("*N")

  RESULTIS 0
$)

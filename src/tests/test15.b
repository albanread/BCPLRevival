LET START() = VALOF
$(
  WRITES("Testing built-in functions*N")

  LET x = 123
  WRITEF("The value of x is %i*N", x)

  NEWLINE()

  WRITES("This is the end of the test.*N");
  FINISH();

  // This part should not be reached
  WRITES("This should not be printed.*N")
  RESULTIS 0
$)

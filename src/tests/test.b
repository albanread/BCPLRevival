// test1.b - A simple BCPL program to test the lexer.
GET "libhdr"

MANIFEST $(
  MAX_ITER = 10
$)

LET start() BE $(
  // This is a block comment.
  /*
    It spans multiple lines.
    Let's check operators and literals.
  */
  LET message = "Hello, BCPL!*n"
  LET pi = 3.14159
  LET count = 0

  FOR i = 1 TO MAX_ITER DO $(
    writes(message)
    count := count + 1
  $)

  // Check new operators
  LET char = message % 1 // Should be 'e'
  LET float_val = pi +. 1.0
  
  IF count = MAX_ITER THEN writes("Loop finished.*n")
$)

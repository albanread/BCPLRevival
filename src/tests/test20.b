
LET funcB(a, b) = VALOF
$(
  LET c = a - b
  RESULTIS c * 2
$)

LET funcA(x, y, z) = VALOF
$(
  LET local_a = x * y + z
  LET local_b = funcB(local_a, z + 5)
  RESULTIS local_a + local_b
$)

LET START() = VALOF
$(
  LET val1 = 10
  LET val2 = 5
  WRITES("Calling funcA...*N")
  LET result = funcA(val1 + 2, val2 * 3, 4)
  WRITEF("Result from funcA is %i*N", result)
  RESULTIS 0
$)

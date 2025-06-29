MANIFEST $(
  TEN = 10
  TWENTY = 20
$)

LET ADD(A, B, C) = A + B + C

LET START() = VALOF
$(
  LET x = ADD(TEN, TWENTY, 30)
  RESULTIS x // Expected result: 60
$)

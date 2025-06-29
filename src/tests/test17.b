GLOBAL $(
  g_val : 1
$)

LET ADD_TO_G(val) BE $(
  g_val := g_val + val
$)

LET START() = VALOF
$(
  g_val := 10
  ADD_TO_G(5)
  RESULTIS g_val // Expected result: 15
$)

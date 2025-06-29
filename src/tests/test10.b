
LET START() = VALOF
$(
  LET x = 10
  REPEAT
  $(
    x := x - 1
  $)
  UNTIL x = 0
  RESULTIS x
$)

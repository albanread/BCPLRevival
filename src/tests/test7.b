
LET START() = VALOF
$(
  LET total = 0
  FOR i = 1 TO 10 DO
  $(
    total := total + i
  $)
  RESULTIS total
$)

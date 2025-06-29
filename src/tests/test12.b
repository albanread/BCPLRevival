LET START() = VALOF
$(
  LET x = 0
  GOTO init

add_ten:
  x := x + 10
  GOTO check

add_five:
  x := x + 5
  GOTO add_ten

init:
  x := 1
  GOTO add_five

check:
  IF x = 16 THEN GOTO finish
  GOTO error // Should not be reached

finish:
  RESULTIS x // Should be 16

error:
  RESULTIS -1
$)

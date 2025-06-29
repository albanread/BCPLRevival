LET START() = VALOF
$(
  LET x = 2
  LET y = 0
  SWITCHON x INTO
  $(
    CASE 1: y := 10
    CASE 2: y := 20
    CASE 3: y := 30
    DEFAULT: y := -1
  $)
  RESULTIS y
$)
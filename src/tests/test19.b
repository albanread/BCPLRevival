LET START() = VALOF
$(
  LET x = 10
  LET y = 20
  LET z = 10

  IF x < y THEN WRITES("x < y*N")
  IF x > y THEN WRITES("x > y*N")
  IF x = z THEN WRITES("x = z*N")
  IF x ~= z THEN WRITES("x ~= z*N")
  IF x <= z THEN WRITES("x <= z*N")
  IF x >= z THEN WRITES("x >= z*N")

  RESULTIS 0
$)

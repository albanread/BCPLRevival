LET START() = VALOF
$(
  LET V = VEC 9 // Creates a vector of 10 elements, indexed 0 to 9
  LET sum = 0
  LET i = 0

  // Initialize the vector
  FOR i := 0 TO 9 DO
    V!i := i * 2

  // Sum the elements of the vector
  FOR i := 0 TO 9 DO
    sum := sum + V!i

  RESULTIS sum // Expected result: (0+2+4+6+8+10+12+14+16+18) = 90
$)

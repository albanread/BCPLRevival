LET START() = VALOF
$(
  LET V = VEC 9 // VEC N allocates a vector of N+1 words, indexed 0 to N

  WRITES("Initializing vector...*N")
  FOR i = 0 TO 9 DO
    V!i := i * 10

  WRITES("Reading and summing vector...*N")
  LET sum = 0
  FOR i = 0 TO 9 DO
    sum := sum + V!i

  WRITEF("Sum of vector elements is %i*N", sum)

  WRITES("Updating V!5...*N")
  V!5 := 100

  WRITEF("New value of V!5 is %i*N", V!5)

  // Recalculate sum
  sum = 0
  FOR i = 0 TO 9 DO
    sum := sum + V!i

  WRITEF("New sum of vector elements is %i*N", sum)

  RESULTIS 0
$)

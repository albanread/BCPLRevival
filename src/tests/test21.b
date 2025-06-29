LET complex_calculation(a,b,c,d,e,f,g,h,i) = VALOF
$(
  // This expression is designed to be complex enough to require
  // more scratch registers than are available in the caller-saved set (x9-x15).
  // This forces the allocator to use callee-saved registers (x19-x27).
  LET temp1 = h / i
  LET temp2 = g * temp1
  LET temp3 = f - temp2
  LET temp4 = e + temp3
  LET temp5 = d / temp4
  LET temp6 = c * temp5
  LET temp7 = b - temp6
  LET result = a + temp7
  RESULTIS result
$)

LET START() = VALOF
$(
  WRITES("Testing scratch register allocation spilling into callee-saved registers...*N")
  // Using values that are unlikely to be optimized away easily.
  LET r = complex_calculation(100, 90, 8, 700, 6, 50, 4, 30, 3)
  WRITEF("Result of complex calculation is %i*N", r)
  RESULTIS 0
$)

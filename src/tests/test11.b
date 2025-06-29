LET START() = VALOF
$(
  LET y = 0
  LET i = 0

  // Test LOOP and BREAK in a WHILE DO loop
  WHILE i < 10 DO
  $(
    i := i + 1
    IF i = 3 THEN LOOP  // Skip iteration when i is 3
    IF i = 7 THEN BREAK // Exit loop when i is 7
    y := y + i
  $)
  // Expected y = 1 + 2 + 4 + 5 + 6 = 18

  LET z := 0
  LET j := 0

  // Test LOOP and BREAK in a REPEAT UNTIL loop
  REPEAT
  $(
    j := j + 1
    IF j = 4 THEN LOOP  // Skip iteration when j is 4
    IF j = 8 THEN BREAK // Exit loop when j is 8
    z := z + j
  $) UNTIL j >= 10
  // Expected z = 1 + 2 + 3 + 5 + 6 + 7 = 24

  // Return a combined value to verify both tests
  // Expected result: 18 * 100 + 24 = 1824
  RESULTIS y * 100 + z
$)

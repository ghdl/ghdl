entity test is
end test;

architecture only of test is
begin  -- only
p: process
  type integerArray is array (0 to 2) of integer;
  variable myArray : integerArray;
begin  -- process p
  myArray(0) := 0;
  myArray(1) := 1;    
  myArray(2) := 2;

  assert myArray(0) = 0 report "TEST FAILED" severity FAILURE;
  assert myArray(1) = 1 report "TEST FAILED" severity FAILURE;
  assert myArray(2) = 2 report "TEST FAILED" severity FAILURE;

  report "TEST PASSED" severity NOTE;
  
  wait;
end process p;
end only;

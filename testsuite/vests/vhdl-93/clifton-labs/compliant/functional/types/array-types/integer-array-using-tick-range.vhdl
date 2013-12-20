entity test is
end test;

architecture only of test is
begin  -- only
p: process
  type integerArray is array (0 to 9) of integer;
  variable myArray : integerArray;
begin  -- process p
  for i in myArray'range loop
    myArray(i) := i;
  end loop;  -- i
  for i in myArray'range loop
    assert myArray(i) = i report "TEST FAILED myArray(i) = " &
      integer'image(myArray(i)) & " - was supposed to be " &
      integer'image(i) severity FAILURE;
  end loop;  -- i
  report "TEST PASSED" severity NOTE;
  wait;
end process p;
end only;

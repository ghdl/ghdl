entity test is
end test;

architecture only of test is
begin  -- only
p: process
  type integerRecord is record
                           foo : integer;
                           bar : integer;
                         end record;
  variable myRecord : integerRecord;
begin  -- process p
  myRecord.foo := 0;
  myRecord.bar := 1;    

  assert myRecord.foo = 0 report "TEST FAILED" severity FAILURE;
  assert myRecord.bar = 1 report "TEST FAILED" severity FAILURE;
  report "TEST PASSED" severity NOTE;
  
  wait;
end process p;
end only;

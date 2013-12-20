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
  myRecord := ( 0, 1 );

  assert myRecord.foo = 0 report "TEST FAILED - 0" severity FAILURE;
  assert myRecord.bar = 1 report "TEST FAILED - 1" severity FAILURE;
  report "TEST PASSED" severity NOTE;
  
  wait;
end process p;
end only;

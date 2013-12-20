entity test is
end test;

architecture only of test is
  procedure proc ( constant l : in integer;
                   constant r : in integer ) is
    type dyn is range l to r;
    constant x : dyn;
  begin
    
  if r = 3 then
    assert x = 1 report "TEST FAILED" severity FAILURE;
  elsif r = 42 then
    assert x = 0 report "TEST FAILED" severity FAILURE;
  end if;
    
  end proc;
begin  -- only
  doit: process
  begin  -- process doit
    proc( 1, 3 );
    proc( 0, 42 );
    report "TEST PASSED";
    wait;
  end process doit;
end only;

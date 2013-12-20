entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
    subtype sub_integer is integer range 42 to 69;
    constant x : sub_integer;
  begin
    assert x = sub_integer'left report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process;   
  
end only;

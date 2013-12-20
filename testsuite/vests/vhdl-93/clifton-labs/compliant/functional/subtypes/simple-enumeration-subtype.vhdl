entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
    subtype sub_boolean is boolean range false to true;
    constant x : sub_boolean;
  begin
    assert x = sub_boolean'left report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process;   
  
end only;

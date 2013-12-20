entity test is
end test;

architecture only of test is
begin  -- only
  only: process
    variable string_variable : string(1 to 5) := "Hello";
    alias string_alias : string(1 to 5) is string_variable;
  begin  -- process
    assert string_alias = "Hello" report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process;
end only;

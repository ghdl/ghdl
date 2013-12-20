entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
    constant string_constant : string := "foobar";
  begin  -- process
    assert string_constant( 1 to 3 ) = "foo" report "TEST FAILED" severity FAILURE;
    assert string_constant( 4 to 6 ) = "bar" report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process;
end only;

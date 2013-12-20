entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
    variable string_var : string( 1 to 6 );
  begin  -- process
    string_var( 1 to 3 ) := "foo";
    string_var( 4 to 6 ) := "bar";
    assert string_var = "foobar" report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process;
end only;

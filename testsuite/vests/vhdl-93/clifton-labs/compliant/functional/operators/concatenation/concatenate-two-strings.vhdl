entity test is
end test;

architecture only of test is

begin  -- only

  doit: process
    variable concatted : string(1 to 6);
  begin  -- process doit
    concatted := "foo" & "bar";

    assert concatted = "foobar" report "TEST FAILED - concatted was not 'foobar'" severity failure;
    assert not(concatted = "foobar") report "TEST PASSED" severity note;
    
    wait;
  end process doit;
end only;

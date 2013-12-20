entity test is
end test;

architecture only of test is
  procedure proc (
    constant a : in bit_vector;
    constant l : in integer ) is
    type dyn is range a'left downto 0;
  begin
    assert dyn'left = l report "TEST FAILED" severity FAILURE;
  end proc;
begin  -- only
  doit: process
  begin  -- process doit
    proc( "0000", 3 );
    proc( "00000", 4 );
    report "TEST PASSED";
    wait;
  end process doit;
end only;

entity test is
end test;

architecture only of test is
begin  -- onlty
  doit: process
    function returns_last( p : bit_vector )
      return bit is
    begin
      return p( p'length - 1 );
    end function;
  begin  -- process doit
    assert returns_last( "00" ) = '0' report "TEST FAILED" severity failure;
    assert returns_last( "11" ) = '1' report "TEST FAILED" severity failure;
    report "TEST PASSED";
    wait;
  end process doit;
end only;

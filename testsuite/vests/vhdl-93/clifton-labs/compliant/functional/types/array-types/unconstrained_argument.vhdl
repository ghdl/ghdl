entity test is
end test;

architecture only of test is
  function get_left (
    constant input_array : bit_vector)
    return bit is
  begin
    return input_array(input_array'left);
  end get_left;
begin  -- only
  process
    constant argument1 : bit_vector( 0 to 3 ) := "0000";    
    constant argument2 : bit_vector( 0 to 4 ) := "11111";
  begin  -- process
    assert get_left( argument1 ) = '0' report "TEST FAILED" severity failure;
    assert get_left( argument2 ) = '1' report "TEST FAILED" severity failure;
    report "TEST PASSED";
    wait;
  end process;
end only;

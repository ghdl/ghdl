entity test is
end test;

architecture only of test is
  type     int_array_char_index_unconstrained is array (character range <>) of integer;
  subtype  int_array_char_index_constrained   is int_array_char_index_unconstrained('0' to '9');
  CONSTANT my_constant : int_array_char_index_constrained := ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 );
begin  -- only
  p: process
  begin  -- process p
    assert my_constant('0') = 0 report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
  wait;
  end process p;
end only;

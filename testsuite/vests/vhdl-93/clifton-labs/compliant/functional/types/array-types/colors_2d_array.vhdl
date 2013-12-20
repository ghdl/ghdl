entity test is
end test;

architecture only of test is
  type colors is ( 'R', 'O', 'Y', 'G', 'B', 'I', 'V', 'X' );
  type color_table_t is array ( 1 to 3, 1 to 3 ) of colors;
  CONSTANT primary_table : color_table_t := (
    -- 'R' 'B' 'Y'
    ( 'R', 'V', 'O' ),  -- 'R'
    ( 'V', 'B', 'G' ),  -- 'B'
    ( 'O', 'G', 'Y' )   -- 'Y'
  );
begin  -- only
  test: process
  begin  -- process test
    assert primary_table( 1, 1 ) = 'R' report "TEST FAILED" severity failure;
    assert primary_table( 1, 2 ) = 'V' report "TEST FAILED" severity failure;
    assert primary_table( 1, 3 ) = 'O' report "TEST FAILED" severity failure;
    assert primary_table( 2, 1 ) = 'V' report "TEST FAILED" severity failure;
    assert primary_table( 2, 2 ) = 'B' report "TEST FAILED" severity failure;
    assert primary_table( 2, 3 ) = 'G' report "TEST FAILED" severity failure;
    assert primary_table( 3, 1 ) = 'O' report "TEST FAILED" severity failure;
    assert primary_table( 3, 2 ) = 'G' report "TEST FAILED" severity failure;
    assert primary_table( 3, 3 ) = 'Y' report "TEST FAILED" severity failure;

    report "TEST PASSED";
    wait;
  end process test;
end only;

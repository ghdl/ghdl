entity test is
end test;

architecture only of test is
  type int_array is array (3 downto 0) of integer;
begin  -- only
  p: process
    variable w, x, y, z : integer := 0;
    variable q : int_array := (3, 2, 1, 0);
  begin  -- process p
    (w, x, y, z) := q;
    assert w = 3 report "TEST FAILED" severity FAILURE;
    assert x = 2 report "TEST FAILED" severity FAILURE;
    assert y = 1 report "TEST FAILED" severity FAILURE;
    assert z = 0 report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
end only;

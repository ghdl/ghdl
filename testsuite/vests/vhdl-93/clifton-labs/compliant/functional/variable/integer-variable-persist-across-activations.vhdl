entity foo is
end foo;

use std.textio.all;

architecture only of foo is
  signal clock : bit;
  signal last_x : integer := 0;
begin  -- only
  process (clock)
    variable x : integer := 0;
  begin  -- process
    last_x <= x;
    x := x + 1;
    wait for 1 fs;
    assert x > last_x report "TEST FAILED" severity FAILURE;
    assert x > 0 report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
  end process;

  process
  begin  -- process
    clock <= '1' after 1 ns,
             '0' after 2 ns,
             '1' after 3 ns;
    wait;
  end process;
end only;

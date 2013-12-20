entity foo is
end foo;

use std.textio.all;

architecture only of foo is
begin  -- only
  process
    variable x : integer := 1;
    variable y : integer := 2;
  begin  -- process
    x := x + y;
    assert x = 3 report "TEST FAILED - x does not equal 1" severity failure;
    assert x /= 3 report "TEST PASSED" severity NOTE;
    wait;
  end process;
end only;

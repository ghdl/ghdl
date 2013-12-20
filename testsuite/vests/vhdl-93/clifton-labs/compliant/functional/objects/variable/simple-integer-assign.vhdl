entity foo is
end foo;

use std.textio.all;

architecture only of foo is
begin  -- only
  process
    variable x : integer := 0;
  begin  -- process
    x := 1;
    assert x = 1 report "TEST FAILED - x does not equal 1" severity failure;
    assert x /= 1 report "TEST PASSED" severity note;
    wait;
  end process;
end only;

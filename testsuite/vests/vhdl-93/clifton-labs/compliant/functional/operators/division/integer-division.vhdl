entity foo is
end foo;

use std.textio.all;

architecture only of foo is
begin  -- only
  process
    variable x : integer := 0;
  begin  -- process
    x := 4/2;
    assert x = 2 report "TEST FAILED - x does not equal 2" severity failure;
    assert x /= 2 report "TEST PASSED" severity note;
    wait;
  end process;
end only;

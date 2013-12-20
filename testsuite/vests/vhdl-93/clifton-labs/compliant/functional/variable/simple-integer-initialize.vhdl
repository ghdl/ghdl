entity foo is
end foo;

use std.textio.all;

architecture only of foo is
begin  -- only
  process
    variable x : integer := 0;
  begin  -- process
    assert x = 0 report "TEST FAILED - x does not equal 1" severity failure;
    report "TEST PASSED" severity note;
    wait;
  end process;
end only;

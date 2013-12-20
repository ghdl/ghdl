entity foo is
end foo;

use std.textio.all;

architecture only of foo is
begin  -- only
  process
    variable x : boolean := false;
  begin  -- process
    x := true;
    assert x = true report "TEST FAILED - x does not equal true" severity failure;
    assert x /= true report "TEST PASSED" severity note;
    wait;
  end process;
end only;

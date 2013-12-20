entity foo is
end foo;

use std.textio.all;

architecture only of foo is
begin  -- only
  process
    variable x : string(1 to 4) := "1234";
  begin  -- process
    assert x'length = 4 report "TEST FAILED - x'length does not equal 4" severity failure;
    assert x'length /= 4 report "TEST PASSED" severity note;
    wait;
  end process;
end only;

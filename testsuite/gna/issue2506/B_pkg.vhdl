library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package B_pkg is

  procedure p;

end package;

package body B_pkg is

  procedure p is
    alias d is << signal DUT.d : std_logic_vector >> ;
  begin

    report "d: 0b" & to_string(d);
    assert d = "01010101"
    severity failure;

  end procedure;

end package body;
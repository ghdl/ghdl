library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test_pkg is
  signal glob_sig : std_logic := '1';
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test_pkg.all;

package test2_pkg is
  procedure test_proc;
end package;
package body test2_pkg is
  procedure test_proc is
  begin
    glob_sig <= force '0';
    wait for 0 ps;
    assert glob_sig = '0' severity error;
    glob_sig <= release;
    wait for 0 ps;
    assert glob_sig = '1' severity error;
  end procedure;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.test2_pkg.all;

entity test is
end entity;

architecture rtl of test is
begin
  p_proc : process
  begin
    test_proc;
    wait;
  end process;
end architecture;

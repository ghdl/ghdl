-- File: test.vhd  (single-file, self-contained)

library ieee;
use ieee.std_logic_1164.all;

package pkg_a is
  type idx_t is (x);
  type arr_t is array (idx_t) of std_logic_vector;
end package;

library ieee;
use ieee.std_logic_1164.all;
package pkg_b is
  type idx_t is (x);
  type arr_t is array (idx_t) of std_logic_vector;
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg_a.all;

entity tb is end;
architecture sim of tb is
  signal a : arr_t(open)(7 downto 0) := (others => x"00");
  signal b : work.pkg_b.arr_t(open)(7 downto 0);
begin
  b <= work.pkg_b.arr_t(a);   -- THIS LINE CRASHES GHDL
  process begin
    wait for 100 ns;
    report "PASS";
    std.env.stop(0);
  end process;
end;

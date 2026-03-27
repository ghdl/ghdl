-- File: test.vhd  (single-file, self-contained)

library ieee;
use ieee.std_logic_1164.all;

package pkg is
  type arr_a is array (natural range <>) of std_logic_vector;
  type arr_b is array (natural range <>) of std_logic_vector;
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity dut is
  port (s_out : out arr_b);  -- unconstrained port
end entity;

architecture rtl of dut is
  signal s : arr_a(0 to 1)(7 downto 0) := (others => (others => '0'));
begin
  s_out <= arr_b(s);  -- CRASH: type conversion to unconstrained port
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity tb is end;
architecture sim of tb is
  signal q : arr_b(0 to 1)(7 downto 0);
begin
  u : entity work.dut port map (s_out => q);
  process begin
    wait for 10 ns;
    report "PASS";
    std.env.stop(0);
  end process;
end;


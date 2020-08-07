library ieee;
use ieee.fixed_float_types.all;
use ieee.fixed_pkg.all;

package filtPkg is
  type u_sfixed_array is array (integer range <>) of u_sfixed;
end package filtPkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.fixed_float_types.all;
use ieee.fixed_pkg.all;
use work.filtpkg.all;

entity filter is
  generic (
    intbits_g: natural;
    decbits_g: natural
  );
  port (
    clk: in std_ulogic;
    c: in u_sfixed_array
  );
end entity filter;

architecture rtl of filter
is
  signal tmp: u_sfixed (intbits_g - 1 downto - decbits_g);
begin
  tmp <= c (0);
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.fixed_float_types.all;
use ieee.fixed_pkg.all;
use work.filtpkg.all;

entity tb is
end entity tb;

architecture test of tb
is
  constant intbits_c: natural := 4;
  constant decbits_c: natural := 6;
  signal clk: std_ulogic := '0';
  signal done: boolean := false;
  signal coeff: u_sfixed_array (0 to 5) (intbits_c - 1 downto - decbits_c) := (others => (others => '0'));
  signal tmp: u_sfixed (intbits_c - 1 downto - decbits_c);
begin
  clk <= transport not clk after 10 ns when not done else '0';
  done <= transport true after 500 ns;
  tmp <= coeff (0);
  filter_1: entity work.filter
    generic map (intbits_g => intbits_c, decbits_g => decbits_c)
    port map (clk => clk, c => coeff);
end architecture test;


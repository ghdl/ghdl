library ieee;
use ieee.std_logic_1164.all;

package filtPkg2 is
  type std_logic_vector_array is array (integer range <>) of std_logic_vector;
end package filtPkg2;

library ieee;
use ieee.std_logic_1164.all;
use work.filtpkg2.all;

entity filter2 is
  generic (
    high_g: natural;
    low_g: natural
  );
  port (
    clk: in std_ulogic;
    c: in std_logic_vector_array
  );
end entity filter2;

architecture rtl of filter2
is
  signal tmp: std_logic_vector (high_g downto low_g);
begin
  tmp <= c (0);
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use work.filtpkg2.all;

entity tb2 is
end entity tb2;

architecture test of tb2
is
  constant high_c: natural := 7;
  constant low_c: natural := 0;
  signal clk: std_ulogic := '0';
  signal done: boolean := false;
  signal coeff: std_logic_vector_array (0 to 5) (high_c downto low_c) := (others => (others => '0'));
  signal tmp: std_logic_vector (high_c downto low_c);
begin
  clk <= transport not clk after 10 ns when not done else '0';
  done <= transport true after 500 ns;
  tmp <= coeff (0);
  filter2_1: entity work.filter2
    generic map (high_g => high_c, low_g => low_c)
    port map (clk => clk, c => coeff);
end architecture test;


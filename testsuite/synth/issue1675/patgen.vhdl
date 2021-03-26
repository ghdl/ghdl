library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pkg.all;

entity patgen is
  port (
    clk : std_logic;
    rst : std_logic;
    bo : out bus_rec_out_t);
end patgen;

architecture behav of patgen is
  signal cnt : unsigned(1 downto 0);
begin
  with cnt select
    bo.dat <= x"01" when "00",
    x"02" when "01",
    x"03" when "10",
    x"05" when "11",
    x"00" when others;

  bo.rst <= rst;
  bo.stb <= not rst;

  process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        cnt <= "00";
      else
        cnt <= cnt + 1;
      end if;
    end if;
  end process;
end behav;


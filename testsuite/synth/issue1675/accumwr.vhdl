library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pkg.all;

entity accumwr is
  port (
    clk : std_logic;
    rst : std_logic;
    en : std_logic;
    res : out std_logic_vector(15 downto 0));
end accumwr;

architecture behav of accumwr is
  signal cnt : unsigned(1 downto 0);
  signal bo : bus_rec_out_t;
begin
  with cnt select
    bo.dat <= x"01" when "00",
    x"02" when "01",
    x"03" when "10",
    x"05" when "11",
    x"00" when others;

  bo.rst <= rst;
  bo.stb <= en;

  process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        cnt <= "00";
      elsif en = '1' then
        cnt <= cnt + 1;
      end if;
    end if;
  end process;

  inst_accum: entity work.accum
    port map (
      clk => clk,
      b_i => bo,
      res => res);
end behav;


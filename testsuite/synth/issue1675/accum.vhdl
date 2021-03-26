library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pkg.all;

entity accum is
  port (
    clk : std_logic;
    b_i : bus_rec_out_t;
    res : out std_logic_vector(15 downto 0));
end accum;

architecture behav of accum is
  signal acc : unsigned(15 downto 0);
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if b_i.rst = '1' then
        acc <= (others => '0');
      elsif b_i.stb = '1' then
        acc <= acc + unsigned(b_i.dat);
      end if;
    end if;
  end process;

  res <= std_logic_vector(acc);
end behav;


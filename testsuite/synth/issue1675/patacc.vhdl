library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pkg.all;

entity patacc is
  port (
    clk : std_logic;
    rst : std_logic;
    res : out std_logic_vector(15 downto 0));
end patacc;

architecture behav of patacc is
  signal bo : bus_rec_out_t;
  signal acc : unsigned(15 downto 0);
begin
  inst: entity work.patgen
    port map (
      clk => clk,
      rst => rst,
      bo => bo);

  process (clk) is
  begin
    if rising_edge(clk) then
      if bo.rst = '1' then
        acc <= (others => '0');
      elsif bo.stb = '1' then
        acc <= acc + unsigned(bo.dat);
      end if;
    end if;
  end process;

  res <= std_logic_vector(acc);
end behav;


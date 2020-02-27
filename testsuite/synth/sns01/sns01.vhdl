library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity sns01 is
  port (a : natural range 0 to 15;
        b : out unsigned (3 downto 0);
        clk : std_logic);
end sns01;

architecture behav of sns01 is
begin
  process (clk)
  begin
    if rising_edge(clk) then
      b <= conv_unsigned (a, 4);
    end if;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity dff03 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic);
end dff03;

architecture behav of dff03 is
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;

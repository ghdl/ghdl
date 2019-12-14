library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dff04 is
  port (r : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic);
end dff04;

architecture behav of dff04 is
  signal q : std_logic_vector(7 downto 0);
begin
  process (clk, q) is
  begin
    if rising_edge (clk) then
      q <= d;
    end if;
    r <= std_logic_vector(unsigned(q) + 1);
  end process;
end behav;

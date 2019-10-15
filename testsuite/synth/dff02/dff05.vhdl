library ieee;
use ieee.std_logic_1164.all;

entity dff05 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic);
end dff05;

architecture behav of dff05 is
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      if d (7) = '1' then
        q (0) <= d (0);
      else
        q (2) <= d (2);
      end if;
    end if;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity comp04 is
  port (p : std_logic_vector (6 downto 0);
        o : out std_logic);
end comp04;

architecture behav of comp04 is
begin
  inst: entity work.sub2
    generic map (width => 8)
    port map (p => p, o => o);
end behav;

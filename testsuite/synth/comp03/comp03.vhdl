library ieee;
use ieee.std_logic_1164.all;

entity comp03 is
  port (p : std_logic_vector (6 downto 0);
        o : out std_logic);
end comp03;

architecture behav of comp03 is
begin
  inst: entity work.sub1
    port map (p => p, o => o);
end behav;

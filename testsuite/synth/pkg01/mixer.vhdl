library ieee;
use ieee.std_logic_1164.all;

entity mixer is
  port (h, l : std_logic_vector(7 downto 0);
        o : out std_logic_vector (7 downto 0));
end mixer;

architecture behav of mixer is
  signal t1 : std_logic_vector (7 downto 0);
begin
  a1: entity work.cmask
    port map (l, t1);
  o <= t1 or h;
end behav;

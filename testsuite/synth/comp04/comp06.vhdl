library ieee;
use ieee.std_logic_1164.all;

entity mand is
  port (l : std_logic_vector;
        r : std_logic_vector := x"7c";
        o : out std_logic_vector);
end mand;

architecture behav of mand is
begin
  o <= l and r;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity comp06 is
  port (v : std_logic_vector (7 downto 0);
        r : out std_logic_vector (7 downto 0));
end;

architecture behav of comp06 is
begin
  dut : entity work.mand
    port map (l => v, o => r);
end behav;

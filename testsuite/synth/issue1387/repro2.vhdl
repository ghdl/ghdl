library ieee;
use ieee.std_logic_1164.all;

entity repro2sub is
  port (a,b : std_logic;
        c : out std_logic;
        p : inout std_logic);
end;

architecture behav of repro2sub is
begin
  c <= a xor b;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
  port (a,b : std_logic;
        c : out std_logic;
        p : inout std_logic);
end;

architecture behav of repro2 is
  signal c1, c2 : std_logic;
begin
  i1: entity work.repro2sub port map (a, b, c1, p);
  i2: entity work.repro2sub port map (a, b, c2, p);
  c <= c2 xor c1;
end behav;

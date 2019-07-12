library ieee;
use ieee.std_logic_1164.all;

entity and6 is
  port (i0, i1, i2, i3, i4, i5 : std_logic;
        o : out std_logic);
end and6;

architecture behav of and6 is
  component and3 is
    port (a, b, c : std_logic;
          o : out std_logic);
  end component;
  signal t1, t2 : std_logic;
begin
  a1: and3
    port map (i0, i1, i2, t1);
  a2: and3
    port map (i3, i4, i5, t2);
  o <= t1 and t2;
end behav;

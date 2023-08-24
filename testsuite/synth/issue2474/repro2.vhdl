library ieee;
use ieee.std_logic_1164.all;

entity tri is
  port (a, en : std_logic;
        o : out std_logic);
end;

architecture behav of tri is
begin
  o <= a when en = '1' else 'Z';
end;

library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
  port (a, b, c : std_logic;
        en1, en2, en3 : std_logic;
        o : out std_logic);
end;

architecture behav of repro2 is
begin
  inst1: entity work.tri
    port map (a, en1, o);
  inst2: entity work.tri
    port map (b, en2, o);
  inst3: entity work.tri
    port map (c, en3, o);
end behav;

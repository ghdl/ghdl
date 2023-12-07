library ieee;
use ieee.std_logic_1164.all;

entity repro1_ent2 is
  port (inp : std_logic;
        o : out std_logic);
end;

architecture behav of repro1_ent2 is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
  port (s : in std_logic := '0');
end repro1;

architecture behav of repro1 is
  signal v : std_logic;
begin
  inst: entity work.repro1_ent2
    port map (inp => v, o => s);
end;

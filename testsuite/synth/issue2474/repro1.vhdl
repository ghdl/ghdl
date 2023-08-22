package pkg1_unused is
  constant msg : string := ('h', 'e', 'l');
end;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg1_unused.all;

entity ent1_unused is
  port (a, b : std_logic;
        o : out std_logic);
end;

architecture behav of ent1_unused is
begin
  assert (a or b) = '1' report msg;
  o <= a and b;
end;

library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
  generic (structural : boolean := false);
  port (a, b : std_logic;
        o : out std_logic);
end;

architecture behav of repro1 is
begin
  g_struct: if structural generate
    inst: entity work.ent1_unused
      port map (a,b, o);
  end generate;
  
  g_behav: if not structural generate
    o <= a and b;
  end generate;
end;

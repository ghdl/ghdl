library ieee;
use ieee.std_logic_1164.all;

package repro1_pkg is
  type rec_t is record
    a : std_logic_vector;
    b : std_logic_vector;
  end record;
end;

library ieee;
use ieee.std_logic_1164.all;
use work.repro1_pkg.all;

entity repro1_ent2 is
  port (inp : rec_t;
        o : out std_logic);
end;

architecture behav of repro1_ent2 is
begin
end;

library ieee;
use ieee.std_logic_1164.all;
use work.repro1_pkg.all;

entity repro1_ent1 is
  port (a : std_logic_vector;
        b : std_logic_vector;
        o : out std_logic);
end;

architecture behav of repro1_ent1 is
begin
  inst: entity work.repro1_ent2
    port map (
      inp.a => a,
      inp.b => b,
      o => o);
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
end repro1;

architecture behav of repro1 is
  signal a : std_logic_vector(3 downto 0);
  signal b : std_logic_vector(7 downto 0);
  signal o : std_logic;
begin
  inst: entity work.repro1_ent1
    port map (a, b, o);
end;

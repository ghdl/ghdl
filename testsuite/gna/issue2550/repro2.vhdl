library ieee;
use ieee.std_logic_1164.all;

package repro2_pkg is
  type arr2_t is array (natural range <>) of std_logic_vector;
end;

library ieee;
use ieee.std_logic_1164.all;
use work.repro2_pkg.all;

entity repro2_ent2 is
  generic (w, l : natural);
  port (inp : arr2_t (l - 1 downto 0)(w - 1 downto 0);
        o : out std_logic);
end;

architecture behav of repro2_ent2 is
begin
end;

library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
end repro2;

architecture behav of repro2 is
  signal a : std_logic_vector(3 downto 0);
  signal o : std_logic;
begin
  inst: entity work.repro2_ent2
    generic map (l => 1, w => 4)
    port map (inp (0) => a, o => o);
end;

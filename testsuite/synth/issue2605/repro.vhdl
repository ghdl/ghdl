library ieee;
use ieee.std_logic_1164.all;

package pkg is
  type myrec is record
    a : std_logic;
    b : std_logic_vector(15 downto 0);
  end record;

  constant init : myrec := ('0', x"5678");
end pkg;

library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity repro_sub is
  generic (mux : natural);
  port (a : std_logic_vector(15 downto 0);
        o : out std_logic_vector(15 downto 0));
end;

architecture behav of repro_sub is
begin
  process (a)
    variable r : myrec := init;
  begin
    r := (a => '0',
          b => a);
    o <= r.b;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro_ext is
  port (a : std_logic_vector(31 downto 0);
        o : out std_logic_vector(31 downto 0));
end;

architecture behav of repro_ext is
  signal s1, s2: std_logic_vector(31 downto 0);
begin
  s1 <= a;
  s2 <= s1;
  o <= s2;
end;

library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a : std_logic_vector(15 downto 0);
        o1, o2 : out std_logic_vector(15 downto 0));
end;

architecture behav of repro is
  signal t : std_logic_vector(31 downto 0);
begin
  inst1: entity work.repro_sub
    generic map (4)
    port map (a, o1);
  inst0: entity work.repro_ext
    port map (x"1111_2222", t);
  inst2: entity work.repro_sub
    generic map (0)
    port map (a, o2);
end;


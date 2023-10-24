library ieee;
use ieee.std_logic_1164.all;

package ex1_pkg1 is
  signal glb : std_logic;
end;

library ieee;
use ieee.std_logic_1164.all;
use work.ex1_pkg1.all;

entity ex1 is
  port (d : std_logic := '0';
        q : out std_logic);
end;

architecture behav of ex1 is
  signal s : std_logic;
begin
  process
  begin
    for i in 1 to 4 loop
      q <= '0';
      wait for 1 ns;
      q <= '1';
      wait for 1 ns;
    end loop;
    wait;
  end process;

  glb <= not s;
end;

      
  

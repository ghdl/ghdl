library ieee;
use ieee.std_logic_1164.all;

entity hier2 is
  port (d : std_logic := '0';
        q : out std_logic);
end;

architecture behav of hier2 is
  --  A nested package
  package pkg is
      signal s : std_logic;
  end pkg;
  use pkg.all;

  signal s2 : std_logic;
begin
  process
  begin
    for i in 1 to 4 loop
      s <= '0';
      wait for 1 ns;
      s <= '1';
      wait for 1 ns;
    end loop;
    wait;
  end process;

  q <= s;

  s2 <= not s;
end;

      
  

library ieee;
use ieee.std_logic_1164.all;

entity insert02 is
  port (a : std_logic_vector (3 downto 0);
        b : std_logic_vector (1 downto 0);
        o0, o1, o2 : out std_logic_vector (3 downto 0));
end insert02;

architecture behav of insert02 is
begin
  process(a, b)
  begin
    o0 <= a;
    o0 (1 downto 0) <= b;

    o1 <= a;
    o1 (2 downto 1) <= b;
    
    o2 <= a;
    o2 (3 downto 2) <= b;
  end process;
end behav;

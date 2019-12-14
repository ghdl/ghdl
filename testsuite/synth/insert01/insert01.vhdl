library ieee;
use ieee.std_logic_1164.all;

entity insert01 is
  port (a : std_logic_vector (3 downto 0);
        b : std_logic;
        o0, o1, o2, o3 : out std_logic_vector (3 downto 0));
end insert01;

architecture behav of insert01 is
begin
  process(a, b)
  begin
    o0 <= a;
    o0 (0) <= b;

    o1 <= a;
    o1 (1) <= b;
    
    o2 <= a;
    o2 (2) <= b;
    
    o3 <= a;
    o3 (3) <= b;
  end process;
end behav;

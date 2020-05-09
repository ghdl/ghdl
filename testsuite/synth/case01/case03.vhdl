library ieee;
use ieee.std_logic_1164.all;

entity case03 is
  port (a : std_logic_vector (4 downto 0);
        o : out std_logic);
end case03;

architecture behav of case03 is
begin
  with a select o <=
    '0' when others;
end behav;

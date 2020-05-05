library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (foo : in  std_logic_vector (3 downto 0);
          bar : out std_logic_vector (7 downto 0));
end issue;

architecture beh of issue is
begin
    bar <= ('0' & foo, others=>'0');
end architecture beh;

library ieee;
use ieee.std_logic_1164.all;

entity sub2 is
  generic
    (width : natural);
  port (p : std_logic_vector (width - 1 downto 0);
        o : out std_logic);
end;

architecture behav of sub2 is
begin
  o <= p (0);
end behav;

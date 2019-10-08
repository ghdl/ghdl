library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity vhd01 is
  port (i1 : std_logic_vector (1 to 1);
        o1 : out std_logic_vector (1 to 1));
end vhd01;

architecture behav of vhd01 is
begin
  o1 <= i1;
end behav;

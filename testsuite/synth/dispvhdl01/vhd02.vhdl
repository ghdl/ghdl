library ieee;
use ieee.std_logic_1164.all;
use work.pkg.all;

entity vhd02 is
  port (i1 : my_rec;
        o1 : out my_rec);
end vhd02;

architecture behav of vhd02 is
begin
  o1 <= i1;
end behav;

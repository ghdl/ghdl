library ieee;
use ieee.std_logic_1164.all;
use work.rec02_pkg.all;

entity rec02 is
  port (p   : in  outer_t;
        oa  : out std_logic;
        ob  : out std_logic;
        oz  : out std_logic);
end rec02;

architecture behav of rec02 is
begin
  oa <= p.i.a;
  ob <= p.i.b;
  oz <= p.z;
end behav;

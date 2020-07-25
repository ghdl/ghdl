library ieee;
use ieee.std_logic_1164.all;

entity repro is
  port (a, b : natural range 0 to 3;
        o : out std_ulogic);
end;

architecture behav of repro is
  type table_2d is array (0 to 3, 0 to 3) of std_ulogic;

  constant table : table_2d := ("0011", "1100", "0101", "1010");
begin
  o <= table(a,b);
end;


library ieee;
use ieee.std_logic_1164.all;
use work.rec03_pkg.all;

entity rec03 is
  port (inp : std_logic;
        o : out myrec);
end rec03;

architecture behav of rec03 is
begin
  o.b <= not inp;
  o.a <= s3 when inp = '0' else s0;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity cmask is
  port (d : std_logic_vector (7 downto 0);
        o : out std_logic_vector (7 downto 0));
end cmask;

use work.pkg.all;

architecture behav of cmask is
begin
  o <= d and mask;
end behav;

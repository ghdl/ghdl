library ieee;
use ieee.std_logic_1164.all;

use work.utility.slv_array;

entity inner is
        generic(
                lower : integer := 3;
                upper : integer := 4
        );
        port(
                test : in slv_array(1 to 2)(lower to upper)
        );
end entity;

architecture test of inner is
begin
end architecture;

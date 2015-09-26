
library ieee;
use ieee.std_logic_1164.all;

entity acomp is
    port (x: in std_ulogic; y: out std_ulogic);
end entity;

architecture aarch of acomp is
begin

    y <= x;

end architecture;

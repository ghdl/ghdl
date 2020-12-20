library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
    port (
        o: out bit
    );
end entity;

architecture arch of ent1 is
begin
    o <= to_bit(std_ulogic' ('L'));
end architecture;

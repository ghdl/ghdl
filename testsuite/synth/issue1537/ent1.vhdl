library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
    port (
        o: out std_ulogic
    );
end entity;

architecture arch of ent1 is
begin
    o <= to_X01(std_ulogic' ('H'));
end architecture;

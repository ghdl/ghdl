library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        o: out std_ulogic
    );
end entity;

architecture arch of ent is
begin
    o <= to_stdulogic('0');
end architecture;

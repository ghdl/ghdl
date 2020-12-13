library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
    port (
        i: in bit_vector(3 downto 0);
        o: out std_ulogic_vector(3 downto 0)
    );
end entity;

architecture arch of ent2 is
begin
    o <= to_stdulogicvector(i);
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
    port (
        o: out std_ulogic_vector(3 downto 0)
    );
end entity;

architecture arch of ent2 is
begin
    o <= to_stdulogicvector(bit_vector'("0101"));
end architecture;

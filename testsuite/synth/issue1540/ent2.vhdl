library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
    port (
        o: out bit_vector(3 downto 0)
    );
end entity;

architecture arch of ent2 is
begin
    o <= to_bitvector(std_ulogic_vector'("01LH"));
end architecture;


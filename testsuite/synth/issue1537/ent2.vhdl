library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        o: out std_ulogic_vector(8 downto 0)
    );
end entity;

architecture arch of ent is
begin
    o <= to_X01(std_ulogic_vector'("U01XZLHW-"));
end architecture;

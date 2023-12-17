entity ent_g is
    generic (
        type T
    );
    port (
        i: in T;
        o: out T
    );
end entity;

architecture arch of ent_g is
begin
    o <= i;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        i: in std_ulogic;
        o: out std_ulogic
    );
end entity;

architecture arch of ent is
begin
    inst: entity work.ent_g
    generic map (
        T => std_ulogic
    )
    port map (
        i => i,
        o => o
    );
end architecture;

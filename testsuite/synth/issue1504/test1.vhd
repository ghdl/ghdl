library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk: in std_ulogic;
        i: in std_ulogic;
        o: out std_ulogic
    );
end entity;

architecture arch of ent is
begin
    o <= i when rising_edge(clk) else unaffected;
end architecture;

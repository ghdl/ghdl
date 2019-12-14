library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        i : in std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0)
    );
end entity;

architecture a of ent is
begin
    o <= i(i'high downto i'low);
end;

library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        i : in std_logic_vector(7 downto 0);
        o : out std_logic_vector(3 downto 0)
    );
end;

architecture a of ent is
    alias high_nibble : std_logic_vector(3 downto 0) is i(7 downto 4);
begin
    o <= high_nibble;
end;


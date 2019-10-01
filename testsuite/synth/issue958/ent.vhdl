library ieee;
use ieee.std_logic_1164.all;

entity ent is
end;

architecture a of ent is
    component c is
        port (
            p : in std_logic_vector(7 downto 0)
        );
    end component;
begin
    inst: component c
        port map (
            p => x"00"
        );
end;


library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
end;

architecture a of ent1 is
    component c is
        port (
            p : in std_logic_vector(7 downto 0)
        );
    end component;
   signal s : std_logic_vector(7 downto 0);
begin
    inst: component c
        port map (
            p => s
        );
    s <= x"01";
end;


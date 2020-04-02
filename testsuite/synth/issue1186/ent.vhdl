library ieee;
use ieee.std_logic_1164.all;

entity ent is
end;

architecture a of ent is
    component c is
        generic (
            G_REAL : real
        );
    end component;
begin
    c_inst: c generic map (G_REAL => 1.5);
end;

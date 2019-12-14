library ieee;
use ieee.std_logic_1164.all;

entity ent is
end;

architecture a of ent is
    function count_ones(vec : std_logic_vector) return natural is
        variable temp : natural := 0;
    begin
        for i in vec'range loop
            if vec(i) then
                temp := temp + 1;
            end if;
        end loop;

        return temp;
    end count_ones;

    constant test : natural := count_ones("10101");
begin
end;

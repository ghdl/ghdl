library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        i : in bit;
        o : out bit
    );
end;

architecture a of ent is
    signal test : std_logic_vector(0 to 7);
begin
    process(i)
    begin
        for x in test'low to test'high loop
        end loop;

        o <= i;
    end process;
end;

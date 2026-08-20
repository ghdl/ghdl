library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk, ce, valid : in std_logic;
        got : out std_logic
    );
end ent;

architecture a of ent is
begin
    process(clk)
    begin
        if rising_edge(clk) then
            got <= '0';
        end if;

        if rising_edge(clk) and ce = '1' then
            if valid then
                got <= '1';
            end if;
        end if;
    end process;
end a;

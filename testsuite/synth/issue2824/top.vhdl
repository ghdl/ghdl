library ieee;
use ieee.std_logic_1164.all;

entity top_level is
end entity;

architecture arch of top_level is
    signal clk: std_ulogic;
begin
    process (clk)
        variable idx: integer range 0 to 1;

        procedure proc(
            variable o: out integer
        ) is
        begin
        end procedure;
    begin
        if rising_edge(clk) then
            proc(idx);
        end if;
    end process;
end architecture;

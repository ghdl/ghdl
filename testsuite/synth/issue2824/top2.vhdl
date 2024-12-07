library ieee;
use ieee.std_logic_1164.all;

entity top2 is
end entity;

architecture arch of top2 is
    signal clk: std_ulogic;
begin
    process (clk)
        variable idx: integer range 0 to 1;

        procedure proc(
            variable o: out integer
        ) is
        begin
          o := 0;
        end procedure;
    begin
        if rising_edge(clk) then
            proc(idx);
        end if;
    end process;
end architecture;

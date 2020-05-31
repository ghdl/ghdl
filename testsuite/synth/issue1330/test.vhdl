library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
        clk : in std_logic;
        write_data : in std_ulogic 
        );
end entity test;

architecture rtl of test is
begin
    test_1: process(clk)
    begin
        if rising_edge(clk) then
                assert write_data = '0' report "bad" severity failure;
        end if;
    end process test_1;
end architecture rtl;

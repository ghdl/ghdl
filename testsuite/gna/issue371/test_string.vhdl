-- VHDL source file
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_string is
end entity;

architecture rtl of test_string is
    signal clk  : std_logic := '1';
    signal test : string (1 to 11) := "Hello World";
begin
    clk <= not clk after 10 ns;
    p_test : process
    begin
        wait for 1 us;
        test <= "hello world";
        wait;
    end process;
end rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  A testbench has no ports.
entity subtype_test_tb is
end subtype_test_tb;

architecture behav of subtype_test_tb is

signal Data1 : unsigned (4-1 downto 0) := x"0";
signal Data2 : unsigned (4-1 downto 0);

begin
    c_pwm : entity work.subtype_test
        port map 
        (
            i_Data => Data1,
            o_Data => Data2
        );
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity testcase is
    generic (
        init_bit : std_logic := '1'
    );
end testcase;

architecture rtl of testcase is
    -- assigning generic to std_logic works OK
    signal test_assign : std_logic := init_bit; 
    -- assigning generic to part of std_logic_vector breaks ghdlsynth
    signal test_assign_vector : std_logic_vector(1 downto 0) := init_bit & "0";
begin
end rtl;

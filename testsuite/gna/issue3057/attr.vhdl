library ieee;
use ieee.std_logic_1164.all;

entity variable_attribute is

end entity variable_attribute;

architecture tb of variable_attribute is

begin

    VARIABLE_ATTRIBUTE_TEST_PROC: process is
        variable test_register : std_logic;
        attribute ATTRIBUTE_FOR_VARIABLE                  : string;
        attribute ATTRIBUTE_FOR_VARIABLE of test_register : variable is "TRUE";
    begin
        wait;
    end process VARIABLE_ATTRIBUTE_TEST_PROC;

end architecture tb;

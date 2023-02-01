library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.env.finish;

entity repro_tb is
end entity repro_tb;

architecture sim of repro_tb is
    type Test_Record_t is record
        test     : std_logic_vector(8 downto 0);
        expected  : std_logic_vector(8 downto 0);
    end record;

    type SLV_vector is array (natural range <>) of std_logic_vector;

    -- Want to create an array of test values without explicitely stating the size
    -- The following line breaks GHDL, if I replace "open" with "0 to 1" everything works as expected.
    constant TEST_VALUES : SLV_vector(open)(7 downto 0) := (
        x"AD",
        x"12"
    );

    constant TEST_VEC : Test_Record_t := (
        test      => "0" & TEST_VALUES(0), -- The concatenation is somehow necessary to reproduce the issue
        expected  => "0" & TEST_VALUES(0)
    );
begin
    test_runner : process
    begin
        assert TEST_VEC.test = TEST_VEC.expected report "Should be equal" severity failure;
        finish;
    end process test_runner;

end architecture sim;

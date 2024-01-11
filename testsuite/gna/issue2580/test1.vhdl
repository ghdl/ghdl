use std.env.all;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test1 is
    generic (
       WIDTH : positive := 8;
       LENGTH : positive := 4
    );
    package test_pkg_def is new work.test_pkg generic map (data_type => std_logic_vector(WIDTH-1 downto 0));
    use test_pkg_def.all;
end test1;
 
architecture behavior of test1 is
begin
    testcase: process
        variable increments : array_data_type(0 to LENGTH-1);
    begin
        initloop: for i in 0 to LENGTH-1 loop
            increments(i) := std_logic_vector(to_unsigned(i, WIDTH));
        end loop initloop;
        printloop: for i in 0 to LENGTH-1 loop
            report "increments(" & integer'image(i) & ") = "  & integer'image(WIDTH) & "X" & '"' & to_hstring(increments(i)) & '"';
        end loop printloop;
        finish;
    end process testcase;
end behavior;


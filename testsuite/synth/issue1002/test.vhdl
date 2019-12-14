library ieee;
use ieee.std_logic_1164.all;
use ieee.math_real.all;

entity test is
    generic (
        BITS : positive := 2
        );
end entity test;

architecture rtl of test is
    constant count : positive := 2 ** BITS - 1;
    subtype node_t is integer range 0 to count;
begin
end;

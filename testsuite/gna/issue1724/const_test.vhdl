library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity const_test is
    generic (
        addr_width : positive := 5
    );
    port (
        clk : in std_logic
    );
end entity;

architecture rtl of const_test is
begin
end architecture rtl;

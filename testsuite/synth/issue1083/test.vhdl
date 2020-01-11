library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port (
        a_in  : in std_ulogic_vector(9 downto 0);
        b_out : out std_ulogic
        );
end entity test;

architecture behaviour of test is
    type array_type_t is array(0 to 1023) of std_ulogic;

    constant ary : array_type_t := (
        2#0000000010# => '1', 
        2#0100000010# => '1', 
        2#1000000010# => '1', 
        2#1100000010# => '1', 
        others => '0'
        );
begin
    b_out <= ary(to_integer(unsigned(a_in)));
end architecture behaviour;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

vunit const_test_vunit2 (const_test(rtl))
{
    constant depth : positive := 2**addr_width;
    type my_arr is array (natural range <>) of std_logic;
    subtype my_word is my_arr(7 downto 0);
}

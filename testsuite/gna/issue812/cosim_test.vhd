library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;

entity cosim_test is
end cosim_test;

architecture rtl of cosim_test is

type ram_type is array(0 to (2**28)-1) of std_logic_vector(127 downto 0);
signal ram : ram_type := (others => (others => '0'));

begin

end rtl;

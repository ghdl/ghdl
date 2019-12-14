library ieee;
use ieee.std_logic_1164.all;

library work;
use work.test_package.all;

entity test is
    generic(
        MEMORY_SIZE : natural := 4096
        );
    port(
        val_in  : in std_logic_vector(63 downto 0)
        );
end entity test;

architecture behaviour of test is
    signal tmp : std_logic_vector(log2(MEMORY_SIZE) - 1 downto 0);
begin
    tmp <= val_in(log2(MEMORY_SIZE) - 1 downto 0);
end architecture behaviour;

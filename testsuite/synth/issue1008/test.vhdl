library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
         addr_in : in std_logic_vector(11 downto 0);
         dat_out : out std_logic_vector(63 downto 0)
         );
end entity test;

architecture behaviour of test is
    type ram_t is array(0 to (4096 / 8) - 1) of std_logic_vector(63 downto 0);
    signal memory : ram_t := (others => (others => '0'));
    signal idx : natural := 0;
begin
    idx <= to_integer(unsigned(addr_in(11 downto 3)));
    dat_out <= memory(idx);
end architecture behaviour;

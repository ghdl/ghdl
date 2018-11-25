library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is end entity;
architecture arch of tb is
	signal reproducer: unsigned(15 downto 0);
begin
	reproducer <= to_unsigned(integer(0.0), 10**7);
end arch;


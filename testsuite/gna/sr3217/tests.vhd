use std.textio.all; --  Imports the standard textio package.
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

--  Defines a design entity, without any ports.
entity tests is
end tests;

architecture behaviour of tests is
begin
	process
		variable l : line;

		variable a : natural := 1;
	begin
		report "a = " & natural'simple_name(a);
		wait;
	end process;
end behaviour;

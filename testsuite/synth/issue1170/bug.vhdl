library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		dummy : in std_ulogic
	);
end bug;

architecture behav of bug is
	constant LEN : positive := 2;
begin
	outer : block
	begin
		gen : for i in 0 to LEN-1 generate
			--empty
		end generate;
	end block;
end architecture;

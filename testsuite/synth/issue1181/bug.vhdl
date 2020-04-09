library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		dummy       : in std_ulogic
	);
end bug;

architecture behav of bug is
begin
	process(all)
		variable index : integer;
	begin
		index := 10;
		if index > 3 then
		--	index := index-1;
		end if;
	end process;

end architecture;

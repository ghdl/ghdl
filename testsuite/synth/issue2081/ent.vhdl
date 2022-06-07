library ieee;
use ieee.std_logic_1164.all;

entity ent is
end entity;

architecture a of ent is
	signal foo : std_logic_vector(7 downto 0);
begin
	process(foo)
	begin
		if foo /= x"00" then
			assert false;
		end if;
	end process;
end architecture;

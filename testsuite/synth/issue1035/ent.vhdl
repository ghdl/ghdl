library ieee;
use ieee.std_logic_1164.all;

entity ent is
end ent;

architecture a of ent is
	constant c : std_logic_vector(7 downto 0) := x"00";
begin
	process(all)
	begin
		case c is
			when others =>
		end case;
	end process;
end a;

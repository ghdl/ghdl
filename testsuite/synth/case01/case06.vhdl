library ieee;
use ieee.std_logic_1164.all;

entity case06 is
	port (
		a : in std_logic;
		b : out std_logic
	);
end entity;

architecture a of case06 is
begin
	process(a)
	begin
		case a is
			when '0' =>
				b <= '0';
			when 'L' =>
				b <= '1';
			when others =>
				b <= 'Z';
		end case;
	end process;
end architecture;

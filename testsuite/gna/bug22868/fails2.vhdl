library ieee;
use ieee.std_logic_1164.all;

entity fails2 is
	port(
		x : in std_logic;
		y : out std_logic_vector(7 downto 0);
		z : out std_logic
	);
end fails2;

architecture a of fails2 is
	component subcomponent is
		port(
			x : in std_logic;
			y : out std_logic_vector(8 downto 0)
		);
	end component;
begin

	s : subcomponent
	port map(
		x => x,
		y(cheese downto 1) => y,
		y(0) => z
	);

end a;

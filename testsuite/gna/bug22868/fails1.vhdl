library ieee;
use ieee.std_logic_1164.all;

entity fails1 is
	generic (
		w : integer := 8
	);
	port(
		x : in std_logic;
		y : out std_logic_vector(7 downto 0);
		z : out std_logic
	);
end entity;

architecture a of fails1 is
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
		y(w downto 1) => y,
		y(0) => z
	);

end a;

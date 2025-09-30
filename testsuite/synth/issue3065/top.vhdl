library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	port (
		clk    : in  std_logic;
		rdaddr : in  std_logic_vector(7 downto 0);
		rddata : out std_logic_vector(7 downto 0)
	);
end top;

architecture synth of top is

	type rom_type is array (0 to 15) of integer range 0 to 255;
	constant rom : rom_type := (
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
	);

	attribute ram_style : string;
	attribute ram_style of rom : constant is "distributed";

begin


	process (CLK)
	begin
		if rising_edge(CLK) then

			rddata <= std_logic_vector(to_unsigned(rom(to_integer(unsigned(rdaddr))), 8));

		end if;
	end process;

end architecture;

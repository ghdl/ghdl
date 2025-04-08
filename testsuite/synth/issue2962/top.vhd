library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
	port (
		sel   : in  std_logic_vector(2 downto 0);
		idata : in  std_logic_vector(7 downto 0);
		odata : out std_logic
	);
end top;

architecture synth of top is

begin

	odata <= idata(to_integer(unsigned(sel)));

end architecture;

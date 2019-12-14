library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Ram is
	generic
	(
		addressWidth : in positive;
		busWidth : in positive;
		size : in positive
	);
	port
	(
		clk : in std_logic;
		address : in unsigned(addressWidth - 1 downto 0);
		writeEnable : in std_logic;
		dataIn : in std_logic_vector(busWidth - 1 downto 0);
		dataOut : out std_logic_vector(busWidth - 1 downto 0)
	);
end Ram;

architecture Behavioral of Ram is
	constant alignment : positive := busWidth / 8;
	constant ramSize : positive := size / alignment;

	type RamType is array(natural range <>) of std_logic_vector(busWidth - 1 downto 0);
	subtype RamRange is natural range 0 to ramSize;

	signal ram : RamType(RamRange);
begin
	process(clk)
		variable index : RamRange;
	begin
		if (rising_edge(clk))
		then
			index := to_integer(address) / alignment;

			if (writeEnable = '1')
			then
				ram(index) <= dataIn;
			end if;

			dataOut <= ram(index);
		end if;
	end process;
end Behavioral;


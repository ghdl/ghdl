library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.AbstractMmPkg.all;

entity testbench is
end entity testbench;

architecture TB of testbench is

	signal rec : AbstractMmRecType(
		writedata(31 downto 0),
		readdata(31 downto 0),
		address(4 downto 0),
		byteen(3 downto 0)
	);

begin

end architecture TB;

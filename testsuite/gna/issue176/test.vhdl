library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity testcase is
	generic ( DATA_WIDTH : natural := 32 );

	port (
		ce        : in  std_logic;
		clk       : in  std_logic
	);
end entity testcase;

architecture behaviour of testcase is

	signal reg_tmode : unsigned(1 downto 0) := "00";

begin


end behaviour;

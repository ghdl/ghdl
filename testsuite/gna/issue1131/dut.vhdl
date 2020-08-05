library IEEE;
use IEEE.Std_Logic_1164.all;

entity DUT is
	generic (
		BUS_WIDTH : integer := 64;
		OTHER_WIDTH : integer := 4
	);
	port (
		Clk : in std_logic;
		Reset : in std_logic
	);
end entity;

architecture Behavioural of DUT is
	type BusT is record
		A : std_logic_vector;
		F : std_logic_vector;
	end record;

	signal BusInst : BusT(
		A(BUS_WIDTH-1 downto 0),
		F(3 downto 0)
	);
begin

end architecture;

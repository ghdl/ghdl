library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity ent is
	port (
		clk : in std_logic;
		i : in std_logic_vector(7 downto 0);

		amount : in integer range 0 to 7;

		const_half : out signed(7 downto 0);
		async_ror  : out std_logic_vector(7 downto 0);
		clocked_left : out unsigned(7 downto 0)
	);
end;

architecture a of ent is
	constant ONE : unsigned(3 downto 0) := x"4" srl 2;
begin
	const_half <= signed(i) sra to_integer(ONE);
	async_ror  <= i ror amount;

	process(clk)
	begin
		if rising_edge(clk) then
			clocked_left <= unsigned(i) sll amount;
		end if;
	end process;
end;

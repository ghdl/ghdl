library ieee;
use ieee.std_logic_1164.all;


library ieee;
use ieee.numeric_std.all;

entity output_split4 is
	port (
		wa0_data : in  std_logic_vector(7 downto 0);
		wa0_addr : in  std_logic_vector(2 downto 0);
		ra0_data : out std_logic_vector(7 downto 0);
		ra0_addr : in  std_logic_vector(2 downto 0);
		wa0_en : in  std_logic;
		clk : in  std_logic
	);
end output_split4;
architecture augh of output_split4 is

	-- Embedded RAM

	type ram_type is array (0 to 7) of std_logic_vector(7 downto 0);
	signal ram : ram_type := (others => (others => '0'));


	-- Little utility functions to make VHDL syntactically correct
	--   with the syntax to_integer(unsigned(vector)) when 'vector' is a std_logic.
	--   This happens when accessing arrays with <= 2 cells, for example.

	function to_integer(B: std_logic) return integer is
		variable V: std_logic_vector(0 to 0);
	begin
		V(0) := B;
		return to_integer(unsigned(V));
	end;

	function to_integer(V: std_logic_vector) return integer is
	begin
		return to_integer(unsigned(V));
	end;

begin

	-- Sequential process
	-- It handles the Writes

	process (clk)
	begin
		if rising_edge(clk) then

			-- Write to the RAM
			-- Note: there should be only one port.

			if wa0_en = '1' then
				ram( to_integer(wa0_addr) ) <= wa0_data;
			end if;

		end if;
	end process;

	-- The Read side (the outputs)

	ra0_data <= ram( to_integer(ra0_addr) );

end architecture;

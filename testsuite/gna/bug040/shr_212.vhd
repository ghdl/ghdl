library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity shr_212 is
	port (
		output : out std_logic_vector(31 downto 0);
		input : in  std_logic_vector(31 downto 0);
		shift : in  std_logic_vector(5 downto 0);
		padding : in  std_logic
	);
end shr_212;

architecture augh of shr_212 is

	signal tmp_padding : std_logic;
	signal tmp_result  : std_logic_vector(32 downto 0);

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

	-- Temporary signals
	tmp_padding <= padding;
	tmp_result <= std_logic_vector(shift_right( unsigned(padding & input), to_integer(shift) ));

	-- The output
	output <= tmp_result(31 downto 0);

end architecture;

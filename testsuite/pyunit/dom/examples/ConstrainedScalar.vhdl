-- Author: Patrick Lehmann
--
-- Constrained scalar subtypes: the range constraint was previously read then discarded.
entity ConstrainedScalar is
end entity ConstrainedScalar;

architecture rtl of ConstrainedScalar is
	constant max : natural := 16;

	type color_t is (red, green, blue, yellow);

	signal s : integer range 0 to 15;
	signal t : natural range 0 to max - 1;
	signal u : color_t range red to blue;

	signal v : bit_vector(7 downto 0);

	-- A range constraint denoted by a range attribute instead of by explicit bounds.
	subtype index_t is natural range v'range;

	type myBit is ('0', '1');
	type myBitVector is array (natural range <>) of myBit;

	function resolveBit (values : myBitVector) return myBit is
	begin
		return values(values'low);
	end function;

	-- A subtype indication that adds a resolution function and no range constraint at all.
	subtype resolved_t is resolveBit myBit;
begin
end architecture rtl;

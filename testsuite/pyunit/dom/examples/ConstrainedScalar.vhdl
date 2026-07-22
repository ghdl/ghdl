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
begin
end architecture rtl;

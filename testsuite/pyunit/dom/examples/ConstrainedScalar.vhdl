-- Author: Patrick Lehmann
--
-- Constrained scalar subtypes: the range constraint was previously read then discarded.
entity ConstrainedScalar is
end entity ConstrainedScalar;

architecture rtl of ConstrainedScalar is
	signal s : integer range 0 to 15;
	signal t : natural range 3 to 9;
begin
end architecture rtl;

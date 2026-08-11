-- Author: Patrick Lehmann
--
-- Alias declarations: object alias with subtype, plain object alias, type alias, and an operator
-- alias with an explicit signature.
package Aliases is
	signal s : bit_vector(7 downto 0);

	alias a : bit_vector(3 downto 0) is s(3 downto 0);
	alias b is s;

	type Integer2 is range 0 to 100;
	alias MyInt is Integer2;

	function add(x, y : integer) return integer;
	alias "+" is add[integer, integer return integer];
end package Aliases;

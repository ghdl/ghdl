-- Author: Patrick Lehmann
--
-- Interface items with and without an explicitly written mode.
entity InterfaceModes is
	generic (
		G0          : natural := 0;
		constant G1 : in natural := 1
	);
	port (
		P0 : bit;
		P1 : in bit;
		P2 : out bit;
		P3 : inout bit;
		P4 : buffer bit
	);
end entity InterfaceModes;

package InterfaceModesPkg is
	procedure Proc(a : integer; b : in integer; variable c : out integer; signal d : inout bit);
	function Func(x : integer; constant y : in integer) return integer;
end package InterfaceModesPkg;

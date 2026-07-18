-- Author: Patrick Lehmann
--
-- A generic package and its instantiation, preceded by library/use clauses.
package GenericPackage is
	generic (
		WIDTH : natural := 8
	);

	constant MAX_VALUE : natural := 2**WIDTH - 1;
end package GenericPackage;

library ieee;
use     ieee.std_logic_1164.all;

package InstantiatedPackage is new work.GenericPackage
	generic map (
		WIDTH => 16
	);

-- Author: Patrick Lehmann
--
-- A collection of utility types and functions.
--
library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;

-- Utility package
package utilities is
	-- Deferred constant to distinguish simulation from synthesis.
	constant IS_SIMULATION : boolean;

end package;

package body utilities is
	function simulation return boolean is
		variable result : boolean := false;
	begin
		-- synthesis translate off
		result := true;
		-- synthesis translate on
		return result;
	end function;

	constant IS_SIMULATION : boolean := simulation;

end package body;

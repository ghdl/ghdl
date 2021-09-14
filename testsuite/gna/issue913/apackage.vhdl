library IEEE;

use IEEE.Std_logic_1164.all;
use IEEE.Numeric_std.all;
package aPackage is

	subtype T_DBYTE is std_logic_vector(7 downto 0);
  	type T_RECORD is record
    	flgA   		: std_logic;
    	flgB	: std_logic;
    	flgC 		: std_logic;
    	flgD	: std_logic;
    	counter	: unsigned(3 downto 0);
  	end record T_RECORD;

  	function to_HKBYTE(inVal : T_RECORD) return T_DBYTE;
end package aPackage;

package body aPackage is

	function to_HKBYTE(inVal : T_RECORD) return T_DBYTE is
	begin
		return (7 downto 4 => std_logic_vector(inVal.counter),
				3 => inVal.flgB, 2 => inVal.flgD,
				1=> inVal.flgC, 0 => inVal.flgA,
				others => '0');
	end to_HKBYTE;

end package body aPackage;

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package pkg is
	attribute Bits        : integer;

	type MY_TYPE is array (5 downto 0) of std_logic_vector(7 downto 0);
	attribute Bits   of MY_TYPE : type is 6 * 8; --Error
	
	type MY_RECORD is record
		sig      : std_logic;
	end record;
	attribute Bits   of MY_RECORD : type is 3 + 3; --No Error
	
end package;

-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;


-- Useful utility functions and types.
package Utilities_pkg is
	type freq is range integer'low to integer'high units
		Hz;
		kHz = 1000 Hz;
		MHz = 1000 kHz;
		GHz = 1000 MHz;
		THz = 1000 GHz;
	end units;

	-- deferred constant
	constant IS_SIMULATION : boolean;

	function ite(condition : boolean; ThenValue : integer; ElseValue : integer) return integer;

	function log2(Value : positive) return positive;

	function bin2onehot(binary : std_logic_vector; bits : natural := 0) return std_logic_vector;
	function bin2onehot(binary : unsigned;         bits : natural := 0) return std_logic_vector;

	function to_index(value : unsigned; max : positive) return natural;
	function to_index(value : natural;  max : positive) return natural;

	component Debouncer is
		generic (
			CLOCK_PERIOD   : time := 10 ns;
			DEBOUNCE_TIME  : time := 3 ms;

			BITS           : positive
		);
		port (
			Clock  : in  std_logic;

			Input  : in  std_logic_vector(BITS - 1 downto 0);
			Output : out std_logic_vector(BITS - 1 downto 0) := (others => '0')
		);
	end component;
end package;


package body Utilities_pkg is
	function simulation return boolean is
		variable result : boolean := FALSE;
	begin
		-- synthesis translate_off
		result := TRUE;
		-- synthesis translate_on
		return result;
	end function;

	-- deferred constant initialization
	constant IS_SIMULATION : boolean := simulation;

	function ite(condition : boolean; ThenValue : integer; ElseValue : integer) return integer is
	begin
		if condition then
			return ThenValue;
		else
			return ElseValue;
		end if;
	end function;

	function log2(Value : positive) return positive is
		variable twosPower : natural := 1;
		variable result    : natural := 0;
	begin
		while (twosPower < Value) loop
			twosPower := twosPower * 2;
			result    := result + 1;
		end loop;
		return result;
	end function;

	function bin2onehot(binary : std_logic_vector; bits : natural := 0) return std_logic_vector is
	begin
		return bin2onehot(unsigned(binary), bits);
	end function;

	function bin2onehot(binary : unsigned; bits : natural := 0) return std_logic_vector is
		variable result : std_logic_vector(2**binary'length - 1 downto 0) := (others => '0');
	begin
		result(to_integer(binary)) := '1';

		if (bits = 0) then
			return result;
		else
			return result(bits - 1 downto 0);
		end if;
	end function;

	function to_index(value : unsigned; max : positive) return natural is
	begin
		return to_index(to_integer(value), max);
	end function;

	function to_index(value : natural; max : positive) return natural is
	begin
		if (value <= max) then
			return value;
		else
			return max;
		end if;
		-- return minimum(value, max);
	end function;
end package body;

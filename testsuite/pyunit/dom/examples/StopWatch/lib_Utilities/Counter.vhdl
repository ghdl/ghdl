-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

use     work.Utilities_pkg.all;


entity Counter is
	generic (
		MODULO : positive;
		BITS   : natural := log2(MODULO)
	);
	port (
		Clock      : in  std_logic;
		Reset      : in  std_logic;
		Enable     : in  std_logic;

		Value      : out unsigned(BITS - 1 downto 0);
		WrapAround : out std_logic
	);
end entity;


architecture rtl of Counter is
	signal CounterValue : unsigned(log2(MODULO) - 1 downto 0) := (others => '0');
begin
	process (Clock)
	begin
		if rising_edge(Clock) then
			if ((Reset or WrapAround) = '1') then
				CounterValue <= (others => '0');
			elsif (Enable = '1') then
				CounterValue <= CounterValue + 1;
			end if;
		end if;
	end process;

	Value      <= resize(CounterValue, BITS);
	WrapAround <= Enable when (CounterValue = MODULO - 1) else '0';
end architecture;

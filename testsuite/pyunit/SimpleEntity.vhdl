-- Author: Patrick Lehmann
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

use     work.Utilities.all;

-- Generic modulo N counter
--
-- This component implements a generic modulo N counter with synchronous reset
-- and enable. It generates a wrap-around strobe signal when it roles from
-- MODULO-1 back to zero.
--
-- .. hint::
--
--    A modulo N counter counts binary from zero to N-1.
--
-- This component uses VHDL-2008 features like readback of ``out`` ports.
entity Counter is
	generic (
		MODULO : positive;                             -- Modulo value.
		BITS   : natural := log2(MODULO)               -- Number of expected output bits.
	);
	port (
		Clock      : in  std_logic;                    -- Component clock
		Reset      : in  std_logic;                    -- Component reset (synchronous)
		Enable     : in  std_logic;                    -- Component enable (synchronous)

		Value      : out unsigned(BITS - 1 downto 0);  -- Current counter value
		WrapAround : out std_logic                     -- Strobe output on change from MODULO-1 to zero
	);
end entity;


-- Synthesizable and simulatable variant of a generic counter.
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

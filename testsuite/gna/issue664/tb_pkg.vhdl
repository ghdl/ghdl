------------------------------------------------------------------
----                                                          ----
----  Content: testbench package                              ----
----                                                          ----
----  Author:  E. Theodore L. Omtzigt                         ----
----           theo@stillwater-sc.com                         ----
----                                                          ----
------------------------------------------------------------------
----                                                          ----
---- Copyright (C) 2017-2018                                  ----
----               E. Theodore L. Omtzigt                     ----
----               theo@stillwater-sc.com                     ----
----                                                          ----
------------------------------------------------------------------

---- A posit is a tapered floating point representation. To compute
---- with posits, the regime and exponent fields need to be consolidated.
---- This process generates a triple (sign, exponent, significant).
---- These triples are the input values to the arithmetic units.

library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

package tb_pkg is

	-- Procedure for clock generation
	procedure clk_gen(signal clk : out std_logic; constant FREQ : real);
	-- How to use
	--architecture sim of tb is
	--		  -- Clock frequency and signal
	--  signal clk_166 : std_logic;
	--  signal clk_125 : std_logic;
	--
	--begin
	--
	--  -- Clock generation with concurrent procedure call
	--  clk_gen(clk_166, 166.667E6);          -- 166.667 MHz clock
	--  clk_gen(clk_125, 125.000E6);          -- 125.000 MHz clock
	--
	--  -- Time resolution show
	--  assert FALSE report "Time resolution: " & time'image(time'succ(0 fs)) severity NOTE;
	--
	--end architecture;

	--- trackError
	function trackError(
		condition : boolean;
		message   : String;
		nrErrors  : integer
	) return integer;

end package;

package body tb_pkg is

	-- Procedure for clock generation
	procedure clk_gen(signal clk : out std_logic; constant FREQ : real) is
		constant PERIOD    : time := 1 sec / FREQ; -- Full period
		constant HIGH_TIME : time := PERIOD / 2; -- High time
		constant LOW_TIME  : time := PERIOD - HIGH_TIME; -- Low time; always >= HIGH_TIME
	begin
		-- Check the arguments
		assert (HIGH_TIME /= 0 fs) report "clk_plain: High time is zero; time resolution to large for frequency" severity FAILURE;
		-- Generate a clock cycle
		loop
			clk <= '1';
			wait for HIGH_TIME;
			clk <= '0';
			wait for LOW_TIME;
		end loop;
	end procedure;

	-- trackError
	function trackError(
		condition : boolean;
		message   : String;
		nrErrors  : integer) return integer is
		variable runningCount : integer;
	begin
		assert (condition) report message;
		if condition = FALSE then
			runningCount := nrErrors + 1;
		else
			runningCount := nrErrors;
		end if;
		return runningCount;
	end function;

end package body;

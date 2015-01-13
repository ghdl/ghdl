-- EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Description:			Prefix AND computation: y(i) <= '1' when x(i downto 0) = (i downto 0 => '1') else '0'
--									This implementation uses carry chains for wider implementations.
-- 
-- Authors:					Thomas B. Preusser
-- =============================================================================
-- Copyright 2007-2014 Technische Universität Dresden - Germany
--										 Chair for VLSI-Design, Diagnostics and Architecture
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--		http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- =============================================================================

library	ieee;
use			ieee.std_logic_1164.all;
use			ieee.numeric_std.all;

library unisim;


entity arith_prefix_and is
	generic (
		N : positive
	);
	port (
		x : in	std_logic_vector(N-1 downto 0);
		y : out std_logic_vector(N-1 downto 0)
	);
end arith_prefix_and;


architecture rtl of arith_prefix_and is
	type T_VENDOR is (VENDOR_XILINX, VENDOR_ALTERA);
	
  constant VENDOR : T_VENDOR  := VENDOR_XILINX;
	
begin
	y(0) <= x(0);
	gen1: if N > 1 generate
		signal	p : unsigned(N-1 downto 1);
	begin
		p(1) <= x(0) and x(1);
		gen2: if N > 2 generate
			p(N-1 downto 2) <= unsigned(x(N-1 downto 2));

			-- Generic Carry Chain through Addition
			genGeneric: if VENDOR /= VENDOR_XILINX generate
				signal	s : std_logic_vector(N downto 1);
			begin
				s <= std_logic_vector(('0' & p) + 1);
				y(N-1 downto 2) <= s(N downto 3) xor ('0' & x(N-1 downto 3));
			end generate genGeneric;

			-- Direct Carry Chain by MUXCY Instantiation
			genXilinx: if VENDOR = VENDOR_XILINX generate
--				component MUXCY
--					port (
--						S	: in	std_logic;
--						DI : in	std_logic;
--						CI : in	std_logic;
--						O	: out std_logic
--					);
--				end component;
				signal	c : std_logic_vector(N-1 downto 0);
			begin
				c(0) <= '1';
				genChain: for i in 1 to N-1 generate
					mux : entity unisim.MUXCY
						port map (
							S	=> p(i),
							DI => '0',
							CI => c(i-1),
							O	=> c(i)
						);
				end generate genChain;
	y(N-1 downto 2) <= c(N-1 downto 2);
			end generate genXilinx;

		end generate gen2;
		y(1) <= p(1);
	end generate gen1;
end rtl;

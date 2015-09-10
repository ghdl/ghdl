-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:				 	Martin Zabel
--									Patrick Lehmann
-- 
-- Module:				 	UART Transmitter
--
-- Description:
-- ------------------------------------
--	TODO
-- 
--	old comments:
--		Serial configuration: 8 data bits, 1 stop bit, no parity
--		
--		bclk = bit clk is rising
--		stb  = strobe, i.e. transmit byte @ din
--		rdy  = ready
--
--
-- License:
-- ============================================================================
-- Copyright 2008-2015 Technische Universitaet Dresden - Germany
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
-- ============================================================================

library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library PoC;
use			PoC.components.all;


entity uart_tx is
  port (
    clk    : in  std_logic;
    rst    : in  std_logic;
    bclk   : in  std_logic;
    stb    : in  std_logic;
    din    : in  std_logic_vector(7 downto 0);
    rdy    : out std_logic;
    txd    : out std_logic
	);
end entity;


architecture rtl of uart_tx is
	type states is (IDLE, TDATA);
	signal state      : states		:= IDLE;
	signal next_state : states;

	-- register
	signal sr					: std_logic_vector(9 downto 1)	:= (others => '0');
	signal sr0				: std_logic											:= '1'; 			        -- current bit to transmit
	signal shift_cnt	: unsigned(3 downto 0)					:= (others => '0');
	signal shift_done	: STD_LOGIC;

	-- control signals
	signal start_tx		: std_logic;
	signal shift_sr		: std_logic;

begin

	process (state, stb, bclk, shift_done)
	begin
		next_state <= state;
		start_tx   <= '0';
		shift_sr   <= '0';

		case state is
			when IDLE =>
				if stb = '1' then
					-- start_tx triggers register initialization
					start_tx   <= '1';
					next_state <= TDATA;
				end if;

			when TDATA =>
				if bclk = '1' then
					-- also shift stop bit into sr0!
					shift_sr <= '1';
					
					if (shift_done = '1') then
						-- condition is true at beginning of sending the stop-bit
						-- synchronization to the bitclk ensures that stop-bit is
						-- transmitted fully
						next_state    <= IDLE;
					end if;
				end if;
			when others => null;
		end case;
	end process;

	process (clk)
	begin
		if rising_edge(clk) then
			if rst = '1' then
				state <= IDLE;
			else
				state <= next_state;
			end if;
			
			if start_tx = '1' then
				-- data, start bit
				sr <= din & '0';
			elsif shift_sr = '1' then
				sr <= '1' & sr(sr'left downto sr'right+1);
			end if;

			if rst = '1' then
				sr0 <= '1';                     -- idle
			elsif shift_sr = '1' then
				sr0 <= sr(1);
			end if;
		end if;
	end process;

	shift_cnt		<= upcounter_next(cnt => shift_cnt, rst => start_tx, en => shift_sr) when rising_edge(clk);
	shift_done	<= upcounter_equal(cnt => shift_cnt, value => 9);
	
	-- outputs
	txd <= sr0;
	rdy <= '1' when state = IDLE else '0';
	
end;

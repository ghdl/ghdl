-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:				 	Martin Zabel
--									Patrick Lehmann
-- 
-- Module:				 	UART Receiver
--
-- Description:
-- ------------------------------------
--	TODO
-- 
--	old comments:
--		Serial configuration: 8 data bits, 1 stop bit, no parity
--		
--		bclk_x8 = bit clock (defined by BAUD rate) times 8
--		dos       = data out strobe, signals that dout is valid, active high for one
--		            cycle 
--		dout      = data out = received byte
--		
--		OUT_REGS:
--		If disabled, then dos is a combinatorial output. Further merging of logic is
--		possible but timing constraints might fail. If enabled, 9 more registers are
--		required. But now, dout toggles only after receiving of full byte.
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


entity uart_rx is
	generic (
		OUT_REGS : boolean
	);
	port (
		clk       : in  std_logic;
		rst       : in  std_logic;
		bclk_x8 	: in  std_logic;
		rxd       : in  std_logic;
		dos       : out std_logic;
		dout      : out std_logic_vector(7 downto 0)
	);
end entity;


architecture rtl of uart_rx is
	type states is (IDLE, RDATA);
	signal state			: states	:= IDLE;
	signal next_state	: states;

	-- registers
	signal rxd_reg1			: std_logic											:= '1';
	signal rxd_reg2			: std_logic											:= '1';
	signal sr						: std_logic_vector(7 downto 0)	:= (others => '0');  -- data only
	signal bclk_cnt			: unsigned(2 downto 0)					:= to_unsigned(4, 3);
	signal shift_cnt		: unsigned(3 downto 0)					:= (others => '0');

	-- control signals
	signal rxd_falling    : std_logic;
	signal bclk_rising    : std_logic;
	signal start_bclk     : std_logic;
	signal shift_sr       : std_logic;
	signal shift_done     : std_logic;
	signal put_data       : std_logic;

begin

  rxd_falling    <= (not rxd_reg1) and rxd_reg2;
  bclk_rising    <= bclk_x8 when (comp_allone(bclk_cnt) = '1') else '0';

  -- shift_cnt count from 0 to 9 (1 start bit + 8 data bits)
	shift_cnt		<= upcounter_next(cnt => shift_cnt, rst => start_bclk, en => shift_sr) when rising_edge(clk);
  shift_done	<= upcounter_equal(cnt => shift_cnt, value => 9);

	bclk_cnt		<= upcounter_next(cnt => bclk_cnt, rst => start_bclk, en => bclk_x8, init => 4) when rising_edge(clk);
	
  process (state, rxd_falling, bclk_x8, bclk_rising, shift_done)
  begin
    next_state <= state;
    start_bclk <= '0';
    shift_sr   <= '0';
    put_data   <= '0';
    
    case state is
      when IDLE =>
        -- wait for start bit
        if (rxd_falling and bclk_x8) = '1' then
          next_state <= RDATA;
          start_bclk <= '1';            -- = rst_shift_cnt
        end if;

      when RDATA =>
        if bclk_rising = '1' then
          -- bit clock keeps running
          if shift_done = '1' then
            -- stop bit reached
            put_data   <= '1';
            next_state <= IDLE;
            
          else
            -- TODO: check start bit?
            shift_sr <= '1';
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

      rxd_reg1 <= rxd;

      if bclk_x8 = '1' then
        -- align to bclk_x8, so when we can easily check for
        -- the falling edge of the start bit
        rxd_reg2 <= rxd_reg1;
      end if;

      if shift_sr = '1' then
        -- shift into MSB
        sr <= rxd_reg2 & sr(sr'left downto 1);
      end if;
    end if;
  end process;

  -- output
  gOutRegs: if OUT_REGS = true generate
    process (clk)
    begin
      if rising_edge(clk) then
        dos  <= put_data and rxd_reg2;  -- check stop bit
        dout <= sr;
      end if;
    end process;
  end generate gOutRegs;

  gNoOutRegs: if OUT_REGS = false generate
    dos  <= put_data and rxd_reg2;      -- check stop bit
    dout <= sr;
  end generate gNoOutRegs;
  
end;

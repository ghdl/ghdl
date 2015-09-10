-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:				 	Martin Zabel
--									Patrick Lehmann
-- 
-- Module:				 	UART Wrapper with Embedded FIFOs and Optional Flow Control
--
-- Description:
-- ------------------------------------
--	Small FIFOs are included in this module, if larger or asynchronous
--	transmit / receive FIFOs are required, then they must be connected
--	externally.
-- 
--	old comments:
--		UART BAUD rate generator
--		bclk	    = bit clock is rising
--		bclk_x8		= bit clock times 8 is rising
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

library PoC;
use			PoC.vectors.all;
use			PoC.physical.all;
use			PoC.components.all;
use			PoC.uart.all;


entity uart_fifo is
	generic (
		CLOCK_FREQ							: FREQ													:= 100 MHz;
		BAUDRATE								: BAUD													:= 115200 Bd;
		FLOWCONTROL							: T_IO_UART_FLOWCONTROL_KIND		:= UART_FLOWCONTROL_NONE;
		TX_MIN_DEPTH						: POSITIVE											:= 16;
		TX_ESTATE_BITS					: NATURAL												:= 1;
		RX_MIN_DEPTH						: POSITIVE											:= 16;
		RX_FSTATE_BITS					: NATURAL												:= 1;
		RX_OUT_REGS							: BOOLEAN												:= FALSE;
		ADD_INPUT_SYNCHRONIZERS	: BOOLEAN												:= TRUE;
		
		SWFC_XON_CHAR						: std_logic_vector(7 downto 0)	:= x"11";		-- ^Q
    SWFC_XON_TRIGGER				: real													:= 0.0625;
		SWFC_XOFF_CHAR					: std_logic_vector(7 downto 0)	:= x"13";		-- ^S
		SWFC_XOFF_TRIGGER				: real													:= 0.75
	);
	port (
		Clock					: in	std_logic;
		Reset					: in	std_logic;

		-- FIFO interface
		TX_put				: in	STD_LOGIC;
		TX_Data				: in	STD_LOGIC_VECTOR(7 downto 0);
		TX_Full				: out	STD_LOGIC;
		TX_EmptyState	: out	STD_LOGIC_VECTOR(TX_ESTATE_BITS - 1 downto 0);
		
		RX_Valid			: out	STD_LOGIC;
		RX_Data				: out	STD_LOGIC_VECTOR(7 downto 0);
		RX_got				: in	STD_LOGIC;
		RX_FullState	: out	STD_LOGIC_VECTOR(RX_FSTATE_BITS - 1 downto 0);
		RX_Overflow		: out	std_logic;
		
		-- External pins
		UART_TX				: out	std_logic;
		UART_RX				: in	std_logic
	);
end entity;


architecture rtl of uart_fifo is

	signal FC_TX_Strobe		: STD_LOGIC;
	signal FC_TX_Data			: T_SLV_8;
	signal FC_TX_got			: STD_LOGIC;
	signal FC_RX_put			: STD_LOGIC;
	signal FC_RX_Data			: T_SLV_8;
	
	signal TXFIFO_Valid		: STD_LOGIC;
	signal TXFIFO_Data		: T_SLV_8;

	signal RXFIFO_Full		: STD_LOGIC;

	signal TXUART_Ready		: STD_LOGIC;
	signal RXUART_Strobe	: STD_LOGIC;
	signal RXUART_Data		: T_SLV_8;

	signal BitClock				: STD_LOGIC;
	signal BitClock_x8		: STD_LOGIC;
  
	signal UART_RX_sync		: STD_LOGIC;
	
begin
	assert FALSE report "uart_fifo: BAUDRATE=: " & to_string(BAUDRATE, 3)						severity NOTE;

	-- ===========================================================================
	-- Transmit and Receive FIFOs
	-- ===========================================================================
	TXFIFO : entity PoC.fifo_cc_got
		generic map (
			D_BITS         => 8,							-- Data Width
			MIN_DEPTH      => TX_MIN_DEPTH,		-- Minimum FIFO Depth
			DATA_REG       => TRUE,						-- Store Data Content in Registers
			STATE_REG      => FALSE,					-- Registered Full/Empty Indicators
			OUTPUT_REG     => FALSE,					-- Registered FIFO Output
			ESTATE_WR_BITS => TX_ESTATE_BITS,	-- Empty State Bits
			FSTATE_RD_BITS => 0								-- Full State Bits
		)
		port map (
			rst  		  => Reset,
			clk  		  => Clock,
			put  		  => TX_put,
			din  		  => TX_Data,
			full 		  => TX_Full,
			estate_wr	=> TX_EmptyState,
			
			valid 		=> TXFIFO_Valid,
			dout  		=> TXFIFO_Data,
			got   		=> FC_TX_got,
			fstate_rd	=> open
		);

  RXFIFO : entity PoC.fifo_cc_got
		generic map (
			D_BITS         => 8,							-- Data Width
			MIN_DEPTH      => RX_MIN_DEPTH,		-- Minimum FIFO Depth
			DATA_REG       => TRUE,						-- Store Data Content in Registers
			STATE_REG      => FALSE,					-- Registered Full/Empty Indicators
			OUTPUT_REG     => FALSE,					-- Registered FIFO Output
			ESTATE_WR_BITS => 0,							-- Empty State Bits
			FSTATE_RD_BITS => RX_FSTATE_BITS	-- Full State Bits
		)
		port map (
			rst  		  => Reset,
			clk  		  => Clock,
			put  		  => FC_RX_put,
			din  		  => FC_RX_Data,
			full 		  => RXFIFO_Full,
			estate_wr	=> open,
			
			valid 		=> RX_Valid,
			dout  		=> RX_Data,
			got   		=> RX_got,
			fstate_rd	=> RX_FullState
		);

	genNOFC : if (FLOWCONTROL = UART_FLOWCONTROL_NONE) generate
		signal Overflow_r					: std_logic					:= '0';
	begin
	
		FC_TX_Strobe	<= TXFIFO_Valid and TXUART_Ready;
		FC_TX_Data		<= TXFIFO_Data;
		FC_TX_got			<= TXFIFO_Valid and TXUART_Ready;

		FC_RX_put			<= RXUART_Strobe;
		FC_RX_Data		<= RXUART_Data;	
		
		Overflow_r	<= ffrs(q => Overflow_r, rst => Reset, set => (RXUART_Strobe and RXFIFO_Full)) when rising_edge(Clock);
		
		RX_Overflow	<= Overflow_r;
	end generate;
	-- ===========================================================================
	-- Software Flow Control
	-- ===========================================================================
	genSWFC : if (FLOWCONTROL = UART_FLOWCONTROL_XON_XOFF) generate
	  constant XON				: std_logic_vector(7 downto 0) := x"11";  -- ^Q
		constant XOFF				: std_logic_vector(7 downto 0) := x"13";  -- ^S

		constant XON_TRIG		: integer	:= integer(SWFC_XON_TRIGGER		* real(2**RX_FSTATE_BITS));
		constant XOFF_TRIG	: integer	:= integer(SWFC_XOFF_TRIGGER	* real(2**RX_FSTATE_BITS));

		signal send_xoff		: std_logic;
		signal send_xon			: std_logic;

		signal set_xoff_transmitted	: std_logic;
		signal clr_xoff_transmitted	: std_logic;
		signal discard_user					: std_logic;

		signal set_overflow					: std_logic;
		
		-- registers
		signal xoff_transmitted			: std_logic;
		
	begin
--		-- send XOFF only once when fill state goes above trigger level
--		send_xoff <= (not xoff_transmitted) when (rf_fs >= XOFF_TRIG) else '0';
--		set_xoff_transmitted <= tx_rdy      when (rf_fs >= XOFF_TRIG) else '0';
--
--		-- send XON only once when receive FIFO is almost empty
--		send_xon <= xoff_transmitted   when (rf_fs = XON_TRIG) else '0';
--		clr_xoff_transmitted <= tx_rdy when (rf_fs = XON_TRIG) else '0';
--
--		-- discard any user supplied XON/XOFF
--		discard_user <= '1' when (tf_dout = SWFC_XON_CHAR) or (tf_dout = SWFC_XOFF_CHAR) else '0';
--
--		-- tx / tf control
--		tx_din <= SWFC_XOFF_CHAR  when (send_xoff = '1') else
--							SWFC_XON_CHAR   when (send_xon  = '1') else
--							tf_dout;
--
--		tx_stb <= send_xoff or send_xon or (tf_valid and (not discard_user));
--		tf_got <= (send_xoff nor send_xon) and
--							tf_valid and tx_rdy;        -- always check tf_valid
--
--		-- rx / rf control
--		rf_put <= (not rf_full) and rx_dos;   -- always check rf_full
--		rf_din <= rx_dout;
--
--		set_overflow <= rf_full and rx_dos;
--		
--		-- registers
--		process (Clock)
--		begin  -- process
--			if rising_edge(Clock) then
--				if (rst or set_xoff_transmitted) = '1' then
--					-- send a XON after reset
--					xoff_transmitted <= '1';
--				elsif clr_xoff_transmitted = '1' then
--					xoff_transmitted <= '0';
--				end if;
--
--				if rst = '1' then
--					overflow <= '0';
--				elsif set_overflow = '1' then
--					overflow <= '1';
--				end if;
--			end if;
--		end process;
	end generate;
	-- ===========================================================================
	-- Hardware Flow Control
	-- ===========================================================================
	genHWFC1 : if (FLOWCONTROL = UART_FLOWCONTROL_RTS_CTS) generate
	
	begin
	
	end generate;
	-- ===========================================================================
	-- Hardware Flow Control
	-- ===========================================================================
	genHWFC2 : if (FLOWCONTROL = UART_FLOWCONTROL_RTR_CTS) generate
	
	begin
	
	end generate;
	
	-- ===========================================================================
	-- BitClock, Transmitter, Receiver
	-- ===========================================================================
	genNoSync : if (ADD_INPUT_SYNCHRONIZERS = FALSE) generate
		UART_RX_sync <= UART_RX;
	end generate;
	genSync: if (ADD_INPUT_SYNCHRONIZERS = TRUE) generate
		sync_i : entity PoC.sync_Bits
			port map (
				Clock  		=> Clock,					-- Clock to be synchronized to
				Input(0)  => UART_RX,				-- Data to be synchronized
				Output(0) => UART_RX_sync		-- synchronised data
			);
	end generate;
	-- ===========================================================================
	-- BitClock, Transmitter, Receiver
	-- ===========================================================================
	bclk : entity PoC.uart_bclk
		generic map (
			CLOCK_FREQ	=> CLOCK_FREQ,
			BAUDRATE		=> BAUDRATE
		)
		port map (
			clk					=> Clock,
			rst					=> Reset,
			bclk				=> BitClock,
			bclk_x8			=> BitClock_x8
		);
	
	TX : entity PoC.uart_tx
		port map (
			clk			=> Clock,
			rst			=> Reset,
			bclk		=> BitClock,
			stb			=> FC_TX_Strobe,
			din			=> FC_TX_Data,
			rdy			=> TXUART_Ready,
			txd			=> UART_TX
		);
		
	RX : entity PoC.uart_rx
		generic map (
			OUT_REGS => RX_OUT_REGS
		)
		port map (
			clk			=> Clock,
			rst			=> Reset,
			bclk_x8	=> BitClock_x8,
			dos			=> RXUART_Strobe,
			dout		=> RXUART_Data,
			rxd			=> UART_RX_sync
		);
end;

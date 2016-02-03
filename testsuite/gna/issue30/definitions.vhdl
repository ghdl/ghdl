library	ieee;
use		ieee.std_logic_1164.all;
--use		ieee.std_logic_arith.all;

package definitions is

-- flag bits
constant	carry_bit:				integer := 0;
constant	add_sub_bit:			integer := 1;
constant	parity_overflow_bit:	integer := 2;
constant	half_carry_bit:	  		integer := 4;
constant	zero_bit:		  		integer := 6;
constant	sign_bit:		  		integer := 7;

-- 8 bit register numbers
constant	B:				  		std_logic_vector(3 downto 0)	:= "0000";
constant	B3:						std_logic_vector(2 downto 0)	:= "000";	-- keep GHDL happy,
																				-- won't accept
																				-- bitslice of
																				-- constant in case
																				-- statements
constant	C:				  		std_logic_vector(3 downto 0)	:= "0001";
constant	C3:						std_logic_vector(2 downto 0)	:= "001";
constant	D:				  		std_logic_vector(3 downto 0)	:= "0010";
constant	D3:						std_logic_vector(2 downto 0)	:= "010";
constant	E:				  		std_logic_vector(3 downto 0)	:= "0011";
constant	E3:						std_logic_vector(2 downto 0)	:= "011";
constant	H:				  		std_logic_vector(3 downto 0)	:= "0100";
constant	H3:						std_logic_vector(2 downto 0)	:= "100";
constant	L:				  		std_logic_vector(3 downto 0)	:= "0101";
constant	L3:						std_logic_vector(2 downto 0)	:= "101";
constant	memory_register:  		std_logic_vector(3 downto 0)	:= "0110";
constant	memory_register3:		std_logic_vector(2 downto 0)	:= "110";
constant	A:			   	  		std_logic_vector(3 downto 0)	:= "0111";
constant	A3:						std_logic_vector(2 downto 0)	:= "111";

constant	one_register:  	  		std_logic_vector(3 downto 0)	:= "1000";	-- fixed constant of
																				-- one at register 8
																				-- in secondary ALU
																				-- register file


constant	zero_register: 	  		std_logic_vector(3 downto 0)	:= "1001";
constant	indexhigh:	   	  		std_logic_vector(3 downto 0)	:= "1010";
constant	indexlow:	   	   		std_logic_vector(3 downto 0)	:= "1011";
constant	bitreg:			   		std_logic_vector(3 downto 0)	:= "1100";
constant	not_bitreg:		   		std_logic_vector(3 downto 0)	:= "1101";
constant	SPhigh:		   	   		std_logic_vector(3 downto 0)	:= "1110";
constant	SPlow:		   	   		std_logic_vector(3 downto 0)	:= "1111";

constant	call_return_interrupt:	std_logic_vector(3 downto 0)	:= "1000";	-- for sending call
																				-- return address
																				-- thru ALU, primary
																				-- register only
constant	flags_register:	   		std_logic_vector(3 downto 0)	:= "1000";	-- for sending flags
																				-- thru ALU -
																				-- multiplexed with
																				-- call return,
																				-- primary register
																				-- only
constant	interrupt_register:		std_logic_vector(3 downto 0)	:= "1000";	-- for sending
																				-- interrupt
																				-- register thru
																				-- ALU - multiplexed
																				-- with call return,
																				-- primary register
																				-- only



-- ALU operation codes
constant	add_operation: 	 		std_logic_vector(4 downto 0)	:= "00000";	-- add without carry
constant	adc_operation: 	 		std_logic_vector(4 downto 0)	:= "00001";	-- add with carry
constant	sub_operation: 			std_logic_vector(4 downto 0)	:= "00010";	-- subtract without
																				-- carry
constant	sbc_operation: 			std_logic_vector(4 downto 0)	:= "00011";	-- subtract with
																				-- carry
constant	and_operation: 			std_logic_vector(4 downto 0)	:= "00100";	-- and
constant	xor_operation: 			std_logic_vector(4 downto 0)	:= "00101";	-- xor
constant	or_operation:  			std_logic_vector(4 downto 0)	:= "00110";	-- or
constant	cmp_operation: 			std_logic_vector(4 downto 0)	:= "00111";	-- compare (subtract
																				-- and discard
																				-- results, set
																				-- flags
constant	rlc_operation: 			std_logic_vector(4 downto 0)	:= "01000";	-- RLC
constant	rrc_operation: 			std_logic_vector(4 downto 0)	:= "01001";	-- RRC
constant	rl_operation: 			std_logic_vector(4 downto 0)	:= "01010";	-- RLA
constant	rr_operation: 			std_logic_vector(4 downto 0)	:= "01011";	-- RRA
constant	daa_operation: 			std_logic_vector(4 downto 0)	:= "01100";	-- DAA
constant	cpl_operation: 			std_logic_vector(4 downto 0)	:= "01101";	-- CPL
constant	scf_operation: 			std_logic_vector(4 downto 0)	:= "01110";	-- SCF
constant	ccf_operation: 			std_logic_vector(4 downto 0)	:= "01111";	-- CCF
constant	sla_operation: 			std_logic_vector(4 downto 0)	:= "10000";	-- SLA
constant	sra_operation: 			std_logic_vector(4 downto 0)	:= "10001";	-- SRA
constant	sll_operation: 			std_logic_vector(4 downto 0)	:= "10010";	-- SLL
constant	srl_operation:	 		std_logic_vector(4 downto 0)	:= "10011";	-- SRL
constant	bit_operation:			std_logic_vector(4 downto 0)	:= "10100";	-- BIT
constant	res_operation: 			std_logic_vector(4 downto 0)	:= "10101";	-- RES
constant	set_operation:			std_logic_vector(4 downto 0)	:= "10110";	-- SET
constant	in16_operation:			std_logic_vector(4 downto 0)	:= "10111";	-- in r, (c)
constant	rld_operation:			std_logic_vector(4 downto 0)	:= "11000";	-- RLD
constant	rrd_operation:			std_logic_vector(4 downto 0)	:= "11001";	-- RRD
constant	blockterm16_operation:	std_logic_vector(4 downto 0)	:= "11010";	-- block instruction
																				-- termination:
																				-- P/V = 0 when
																				-- BC = 0

-- ALU operation flags masks - the ones that change are listed, others are masked out
constant	alu_mask:				std_logic_vector(7 downto 0)	:= (	carry_bit => '1',
																			add_sub_bit => '1',
																			parity_overflow_bit => '1',
																			half_carry_bit => '1',
																			zero_bit => '1',
																			sign_bit => '1',
																			others => '0'
																			);

-- Block operation load masks
constant	block_load_mask:		std_logic_vector(7 downto 0)	:= (	add_sub_bit => '1',
																			parity_overflow_bit => '1',
																			half_carry_bit => '1',
																			others => '0'
																			);

constant	block_compare_mask1:	std_logic_vector(7 downto 0)	:= (	add_sub_bit => '1',
																			half_carry_bit => '1',
																			zero_bit => '1',
																			sign_bit => '1',
																			others => '0'
																			);

constant	block_compare_mask2:	std_logic_vector(7 downto 0)	:= (	parity_overflow_bit => '1',
																			others => '0'
																			);

constant	block_io_mask:			std_logic_vector(7 downto 0)	:= (	add_sub_bit => '1',
																			zero_bit => '1',
																			others => '0'
																			);

-- bit masks for bit oriented instructions
constant	bit7mask:	std_logic_vector(7 downto 0)	:= (7 => '1', others => '0');
constant	bit6mask:	std_logic_vector(7 downto 0)	:= (6 => '1', others => '0');
constant	bit5mask:	std_logic_vector(7 downto 0)	:= (5 => '1', others => '0');
constant	bit4mask:	std_logic_vector(7 downto 0)	:= (4 => '1', others => '0');
constant	bit3mask:	std_logic_vector(7 downto 0)	:= (3 => '1', others => '0');
constant	bit2mask:	std_logic_vector(7 downto 0)	:= (2 => '1', others => '0');
constant	bit1mask:	std_logic_vector(7 downto 0)	:= (1 => '1', others => '0');
constant	bit0mask:	std_logic_vector(7 downto 0)	:= (0 => '1', others => '0');

-- address bus selector
constant	address_bus_source_BC:			std_logic_vector(3 downto 0)	:= x"0";
constant	address_bus_source_DE:			std_logic_vector(3 downto 0)	:= x"1";
constant	address_bus_source_HL:			std_logic_vector(3 downto 0)	:= x"2";
constant	address_bus_source_SP:			std_logic_vector(3 downto 0)	:= x"3";
constant	address_bus_source_PC:			std_logic_vector(3 downto 0)	:= x"4";
constant	address_bus_source_operand:		std_logic_vector(3 downto 0)	:= x"5";
constant	address_bus_source_operand1:	std_logic_vector(3 downto 0)	:= x"6";
constant	address_bus_source_port8:		std_logic_vector(3 downto 0)	:= x"7";
constant	address_bus_source_index:		std_logic_vector(3 downto 0)	:= x"8";

-- program counter selector
constant	pc_source_next:					std_logic_vector(3 downto 0)	:= x"0";	-- PC + 1
constant	pc_source_operand:				std_logic_vector(3 downto 0)	:= x"1";	-- operand
constant	pc_source_HL:					std_logic_vector(3 downto 0)	:= x"2";	-- PCHL
constant	pc_source_return:				std_logic_vector(3 downto 0)	:= x"3";	-- return
																						-- address
constant	pc_source_next_next:			std_logic_vector(3 downto 0)	:= x"4";	-- PC + 2
constant	pc_source_rst:					std_logic_vector(3 downto 0)	:= x"5";	-- RST nn
constant	pc_source_index_register:		std_logic_vector(3 downto 0)	:= x"6";	-- PCIX and PCIY
constant	pc_source_jr:					std_logic_vector(3 downto 0)	:= x"7";	-- JR offset
constant	pc_source_block_repeat:			std_logic_vector(3 downto 0)	:= x"8";	-- for
																						-- interrupted
																						-- block repeats
constant	pc_source_reset:				std_logic_vector(3 downto 0)	:= x"f";	-- sets PC
																						-- vector
																						-- after reset

-- initial program counter.  Zilog spec says it is always zero, but often autojump hardware is
-- implemented to change this.  I have the luxury of specifying the initial program counter as I see
-- fit.
constant	PC_start_address:				std_logic_vector(15 downto 0)	:= x"0000";

-- SP source mux input definitins
constant	SPsource_databus:				std_logic_vector(1 downto 0)	:= "00";	-- select
																						-- databus
constant	SPsource_increment:				std_logic_vector(1 downto 0)	:= "01";	-- select SP + 1
constant	SPsource_decrement:				std_logic_vector(1 downto 0)	:= "10";	-- select SP - 1

-- data output mux selectors
constant	data_out_selector_databus:			std_logic_vector(2 downto 0)	:= "000";	-- select
																							-- databus
constant	data_out_selector_H:				std_logic_vector(2 downto 0)	:= "001";	-- select
																							-- temporary
																							-- register
																							-- for
																							-- register
																							-- H
constant	data_out_selector_L:				std_logic_vector(2 downto 0)	:= "010";	-- select
																							-- temporary
																							-- register
																							-- for
																							-- register
																							-- L
constant	data_out_selector_indexhigh:		std_logic_vector(2 downto 0)	:= "011";	-- select
																							-- temporary
																							-- register
																							-- for
																							-- register
																							-- IXhigh
constant	data_out_selector_indexlow:			std_logic_vector(2 downto 0)	:= "100";	-- select
																							-- temporary
																							-- register
																							-- for
																							-- register
																							-- IXlow
constant	data_out_selector_rrd_rld_output:	std_logic_vector(2 downto 0)	:= "101";	-- select
																							-- secondary
																							-- ALU
																							-- output
																							-- (RLD and
																							-- RRD)

-- select among return address, flags, or interrupt vector register to go through the ALU.
constant	selectRetAddr:					std_logic_vector(1 downto 0)	:= "00";	-- select return
																						-- address
constant	selectFlags:					std_logic_vector(1 downto 0)	:= "01";	-- select flags
constant	selectInterruptVector:			std_logic_vector(1 downto 0)	:= "10";	-- select
																						-- interrupt
																						-- vector
																						-- register

-- operand register selection
constant	operandSelectLow:				std_logic	:= '0';	-- selects operand register low byte
constant	operandSelectHigh:				std_logic	:= '1';	-- selects operand register bigh byte

-- assertion and deassertion of control lines
constant	assert_m1:						std_logic	:= '1';
constant	deassert_m1:					std_logic	:= '0';
constant	assert_write:					std_logic	:= '1';
constant	deassert_write:					std_logic	:= '0';
constant	assert_read:					std_logic	:= '1';
constant	deassert_read:					std_logic	:= '0';
constant	assert_iorq:					std_logic	:= '1';
constant	deassert_iorq:					std_logic	:= '0';
constant	assert_mreq:					std_logic	:= '1';
constant	deassert_mreq:					std_logic	:= '0';

-- Index register selection
constant	SelectIndexIX:					std_logic	:= '0';
constant	SelectIndexIY:					std_logic	:= '1';

-- Return address byte selection
constant	RetAddrLow:						std_logic	:= '0';
constant	RetAddrHigh:					std_logic	:= '1';

-- Enable and disable writing to instruction register
constant	disableOpcodeWrite:				std_logic	:= '0';
constant	enableOpcodeWrite:	   			std_logic	:= '1';

-- Enable and disable XCHG hardware
constant	disableXCHG:					std_logic	:= '0';
constant	enableXCHG:			   			std_logic	:= '1';

-- For master and slave control of the address, data and control busses
constant	masterControl:					std_logic	:= '1';
constant	slaveControl:					std_logic	:= '0';

constant	deassert_halted:				std_logic	:= '0';
constant	assert_halted:					std_logic	:= '1';

-- For control of source of ALU operation
constant	selectVHDL_ALU_operation:		std_logic	:= '0';
constant	selectOpcode_ALU_operation:		std_logic	:= '1';

-- for source of primary and secondary ALU operand registers
constant	selectOpcodeRegister:			std_logic	:= '0';
constant	selectVHDLRegister:				std_logic	:= '1';

-- for register saving
constant	save:							std_logic	:= '1';
constant	DontSave:						std_logic	:= '0';

-- for choosing source of flags data
constant	ALUflags:						std_logic	:= '0';
constant	POPflags:						std_logic	:= '1';

-- for general clock enable and disable
constant	clockEnable:					std_logic	:= '1';
constant	clockDisable:					std_logic	:= '0';

-- for invalid instruction detector
constant	safe:							std_logic	:= '0';
constant	fail:							std_logic	:= '1';

-- for interrupt modes
constant	IM_0:							std_logic_vector(1 downto 0)	:= "00";
constant	IM_1:							std_logic_vector(1 downto 0)	:= "01";
constant	IM_2:							std_logic_vector(1 downto 0)	:= "10";

constant	width_is_8:						positive := 8;

-- State numbers.  Done this way so state numbers can be stored and used as a return address

-- common to all opcodes


constant	initialise:					std_logic_vector(11 downto 0)	:= x"000";	-- initialise
																					-- processor,
																	 				-- enters on
																	 				-- rising edge of
																	 				-- clk_out
constant	initialise1:				std_logic_vector(11 downto 0)	:= x"001";	-- second
																					-- initialisation
																	 				-- state
constant	initialise2:				std_logic_vector(11 downto 0)	:= x"002";	-- third
																					-- initialisation
																	 				-- state
constant	initialise3:				std_logic_vector(11 downto 0)	:= x"003";	-- fourth
																					-- initialisation
																	 				-- state
constant	opcode:						std_logic_vector(11 downto 0)	:= x"004";	-- assert pc address
																					-- drivers and
																					-- m1n = '0'
																					-- for 1st opcode
																					-- byte, rising edge
																					-- of clock, done
																					-- in last state of
																					-- previous
																					-- instruction
constant	opcode_mreq:				std_logic_vector(11 downto 0)	:= x"005";	-- assert
																					-- mreqn = '0' and
																					-- rdn = '0' on
																					-- falling edge of
																					-- clock
constant	opcode_latch:				std_logic_vector(11 downto 0)	:= x"006";	-- latch opcode byte
																					-- on next rising
																					-- edge of clock
																					-- with waitn = '1',
																					-- rising edge of
																					-- clock
constant	decode_opcode:				std_logic_vector(11 downto 0)	:= x"007";	-- decode first
																					-- opcode
constant	invalid:					std_logic_vector(11 downto 0)	:= x"008";	-- state name for
																					-- invalid return
																					-- state and illegal
																					-- instruction

-- states for BUSRQ handling
constant	busrq:						std_logic_vector(11 downto 0)	:= x"009";

-- New PC state for use with jr, jp, ret, call, rst
constant	NewPC:						std_logic_vector(11 downto 0)	:= x"00f";

-- opcodes in order presented by z80.info/decoding.  Number of states (initially) conforms
-- to number of clock cycles as advertised in the Z80 data sheet.  States are added because
-- I process information on both positive and negative transitions of the clock.  These
-- will be removed if they are not needed. Memory and port I/O operations are always
-- initiated at the positive going edge of the clock.  Instructions that do not appear here
-- are processed entirely during the decoding phase of operation.

-- NOP states, 4 clock cycles, all required for timing loops, 1 m1 cycle
constant	nop4:						std_logic_vector(11 downto 0)	:= x"010";	-- instruction
																					-- origin + 3 rising
																					-- edges
constant	nop5:						std_logic_vector(11 downto 0)	:= x"011";	-- instruction
																					-- origin + 4 rising
																					-- edges

-- DJNZ, 8/13 cycles (met, not met), 1 m1 cycle
constant	djnz4:						std_logic_vector(11 downto 0)	:= x"030";

-- JR, 12 cycles, 1 m1 cycle
constant	jr4:						std_logic_vector(11 downto 0)	:= x"040";
constant	jr5:						std_logic_vector(11 downto 0)	:= x"041";
constant	jr6:						std_logic_vector(11 downto 0)	:= x"042";
constant	jr7:						std_logic_vector(11 downto 0)	:= x"043";

-- JR conditional, 12/7 cycles (met/not met), 1 m1 cycle
-- need one state to test condition, transfer control to jr code
-- Number of cycles = one or two more than jr
constant	jrcc4:						std_logic_vector(11 downto 0)	:= x"050";
constant	jrcc5:						std_logic_vector(11 downto 0)	:= x"051";

-- LD rp, immediate, 10 cycles, 1 m1 cycle
constant	ldrpi4:						std_logic_vector(11 downto 0)	:= x"060";
constant	ldrpi5:						std_logic_vector(11 downto 0)	:= x"061";

-- ADD HL, rp, 11 clock cycles, 1 m1 cycle
constant	addhlrp4:					std_logic_vector(11 downto 0)	:= x"070";
constant	addhlrp5:					std_logic_vector(11 downto 0)	:= x"071";

-- LDAX rp, 7 cycles, 1 m1 cycle
constant	ldax4:						std_logic_vector(11 downto 0)	:= x"080";
constant	ldax5:						std_logic_vector(11 downto 0)	:= x"081";

-- STAX rp, 7 cycles, 1 m1 cycle
constant	stax4:						std_logic_vector(11 downto 0)	:= x"090";
constant	stax5:						std_logic_vector(11 downto 0)	:= x"091";

-- LDA nn, 13 cycles, 1 m1 cycle
constant	lda4:						std_logic_vector(11 downto 0)	:= x"0a0";
constant	lda5:						std_logic_vector(11 downto 0)	:= x"0a1";
constant	lda6:						std_logic_vector(11 downto 0)	:= x"0a2";
constant	lda7:						std_logic_vector(11 downto 0)	:= x"0a3";

-- STA nn, 13 cycles, 1 m1 cycle
constant	sta4:						std_logic_vector(11 downto 0)	:= x"0b0";
constant	sta5:						std_logic_vector(11 downto 0)	:= x"0b1";


-- LHLD (nn), 16 cycles, 1 m1 cycle
constant	ldhl4:						std_logic_vector(11 downto 0)	:= x"0c0";
constant	ldhl5:						std_logic_vector(11 downto 0)	:= x"0c1";
constant	ldhl6:						std_logic_vector(11 downto 0)	:= x"0c2";
constant	ldhl7:						std_logic_vector(11 downto 0)	:= x"0c3";
constant	ldhl8:						std_logic_vector(11 downto 0)	:= x"0c4";

-- SHLD (nn), 16 cycles, 1 m1 cycle
constant	sthl4:						std_logic_vector(11 downto 0)	:= x"0d0";
constant	sthl5:						std_logic_vector(11 downto 0)	:= x"0d1";
constant	sthl6:						std_logic_vector(11 downto 0)	:= x"0d2";
constant	sthl7:						std_logic_vector(11 downto 0)	:= x"0d3";

-- 16 bit increment/decrement, 6 cycles, 1 m1 cycle
constant	incdec16_4:					std_logic_vector(11 downto 0)	:= x"0e0";
constant	incdec16_5:					std_logic_vector(11 downto 0)	:= x"0e1";
constant	incdec16_6:					std_logic_vector(11 downto 0)	:= x"0e2";
constant	incdec16_7:					std_logic_vector(11 downto 0)	:= x"0e3";

-- 8 bit register/memory increment/decrement, 11 cycles, 1 m1 cycle
constant	incdec8_4:					std_logic_vector(11 downto 0)	:= x"0f0";

-- 8 bit load immediate, 7 cycles, 1 m1 cycle
constant	ldi4:						std_logic_vector(11 downto 0)	:= x"100";

-- DAA, 4 cycles, 1 m1 cycle
constant	daa4:						std_logic_vector(11 downto 0)	:= x"110";
constant	daa5:						std_logic_vector(11 downto 0)	:= x"111";
constant	daa6:						std_logic_vector(11 downto 0)	:= x"112";
constant	daa7:						std_logic_vector(11 downto 0)	:= x"113";

-- SCF/CCF, 4 cycles, 1 m1 cycle
-- main processing done at instruction decoder stage
constant	scf_ccf_save:				std_logic_vector(11 downto 0)	:= x"120";

-- inter-register 8 bit loading, 4 cycles, 1 m1 cycle
constant	irld4:						std_logic_vector(11 downto 0)	:= x"130";
constant	irld5:						std_logic_vector(11 downto 0)	:= x"131";

-- HALT, 4 cycles, 1 m1 cycle, may trim this to three
-- cycles initially plus one cycle per instruction thereafter
constant	halt4:						std_logic_vector(11 downto 0)	:= x"140";
constant	halt5:						std_logic_vector(11 downto 0)	:= x"141";
constant	halt6:						std_logic_vector(11 downto 0)	:= x"142";
constant	halt7:						std_logic_vector(11 downto 0)	:= x"143";

-- alu operations on registers, 4 cycles, 1 m1 cycle
constant	alu4:						std_logic_vector(11 downto 0)	:= x"150";

-- POP, 10 cycles, 1 m1 cycle
constant	pop4:						std_logic_vector(11 downto 0)	:= x"160";
constant	pop5:						std_logic_vector(11 downto 0)	:= x"161";
constant	pop6:						std_logic_vector(11 downto 0)	:= x"162";
constant	pop7:						std_logic_vector(11 downto 0)	:= x"163";
constant	pop8:						std_logic_vector(11 downto 0)	:= x"164";

-- RET unconditional, 10 cycles, 1 m1 cycle
constant	ret4:						std_logic_vector(11 downto 0)	:= x"170";
constant	ret5:						std_logic_vector(11 downto 0)	:= x"171";
constant	ret6:						std_logic_vector(11 downto 0)	:= x"172";
constant	ret7:						std_logic_vector(11 downto 0)	:= x"173";
constant	ret8:						std_logic_vector(11 downto 0)	:= x"174";
constant	ret9:						std_logic_vector(11 downto 0)	:= x"175";

-- JP HL, 4 cycles,1 m1 cycle
constant	jphl4:						std_logic_vector(11 downto 0)	:= x"180";

-- LD SP, HL, 6 cycles, 1 m1 cycle
constant	sphl4:						std_logic_vector(11 downto 0)	:= x"190";

-- JP conditional, 10 cycles met or not, 1 m1 cycle
-- use one state to determine if ret is to be executed, then transfer to JP unconditional if so.
constant	jpcc4:						std_logic_vector(11 downto 0)	:= x"1a0";
constant	jpcc5:						std_logic_vector(11 downto 0)	:= x"1a1";

-- JP unconditional, 10 cycles, 1 m1 cycle
constant	jp4:						std_logic_vector(11 downto 0)	:= x"1b0";
constant	jp5:						std_logic_vector(11 downto 0)	:= x"1b1";
constant	jp6:						std_logic_vector(11 downto 0)	:= x"1b2";

-- CB prefix, must obtain next instruction byte
constant	cb4:						std_logic_vector(11 downto 0)	:= x"1c0";
constant	cb5:						std_logic_vector(11 downto 0)	:= x"1c1";

-- save results from CB prefixed opcodes other than BIT, SET, and RES
constant	bitsave:					std_logic_vector(11 downto 0)	:= x"1e0";

-- common state for save and load 16 bit registers with ED prefix
constant	rp16io:						std_logic_vector(11 downto 0)	:= x"1f0";

-- BIT
constant	bit6:						std_logic_vector(11 downto 0)	:= x"200";
constant	bit7:						std_logic_vector(11 downto 0)	:= x"201";

-- RES
constant	res6:						std_logic_vector(11 downto 0)	:= x"210";

-- SET
constant	set6:						std_logic_vector(11 downto 0)	:= x"220";

-- end of CB prefixed opcodes

-- 8 bit output of accumulator to 8 bit port address, 11 cycles, 1 m1 cycle
constant	out4:						std_logic_vector(11 downto 0)	:= x"230";
constant	out5:						std_logic_vector(11 downto 0)	:= x"231";
constant	out6:						std_logic_vector(11 downto 0)	:= x"232";
constant	out7:						std_logic_vector(11 downto 0)	:= x"233";
constant	out8:						std_logic_vector(11 downto 0)	:= x"234";

-- 8 bit input of accumulator from 8 bit port address, 11 cycles, 1 m1 cycle
constant	in4:						std_logic_vector(11 downto 0)	:= x"240";
constant	in5:						std_logic_vector(11 downto 0)	:= x"241";
constant	in6:						std_logic_vector(11 downto 0)	:= x"242";
constant	in7:						std_logic_vector(11 downto 0)	:= x"243";

-- EX (SP), HL, 19 cycles, 1 m1 cycle
constant	xthl4:						std_logic_vector(11 downto 0)	:= x"250";
constant	xthl5:						std_logic_vector(11 downto 0)	:= x"251";
constant	xthl6:						std_logic_vector(11 downto 0)	:= x"252";
constant	xthl7:						std_logic_vector(11 downto 0)	:= x"253";
constant	xthl8:						std_logic_vector(11 downto 0)	:= x"254";
constant	xthl9:						std_logic_vector(11 downto 0)	:= x"255";
constant	xthl10:						std_logic_vector(11 downto 0)	:= x"256";

-- DI, 4 cycles, 1 m1 cycle
constant	di4:						std_logic_vector(11 downto 0)	:= x"270";
constant	di5:						std_logic_vector(11 downto 0)	:= x"271";
constant	di6:						std_logic_vector(11 downto 0)	:= x"272";
constant	di7:						std_logic_vector(11 downto 0)	:= x"273";

-- EI, 4 cycles, 1 m1 cycle
constant	ei4:						std_logic_vector(11 downto 0)	:= x"280";
constant	ei5:						std_logic_vector(11 downto 0)	:= x"281";
constant	ei6:						std_logic_vector(11 downto 0)	:= x"282";
constant	ei7:						std_logic_vector(11 downto 0)	:= x"283";

-- PUSH, 10 cycles, 1 m1 cycle
constant	push4:						std_logic_vector(11 downto 0)	:= x"2a0";
constant	push5:						std_logic_vector(11 downto 0)	:= x"2a1";
constant	push6:						std_logic_vector(11 downto 0)	:= x"2a2";
constant	push7:						std_logic_vector(11 downto 0)	:= x"2a3";

-- CALL unconditional, 17 clock cycles, 1 m1 cycle
constant	call4:						std_logic_vector(11 downto 0)	:= x"2b0";
constant	call5:						std_logic_vector(11 downto 0)	:= x"2b1";
constant	call6:						std_logic_vector(11 downto 0)	:= x"2b2";
constant	call7:						std_logic_vector(11 downto 0)	:= x"2b3";
constant	call8:						std_logic_vector(11 downto 0)	:= x"2b4";

-- end of DD prefixed opcodes

-- ED prefix, must obtain next instruction byte
constant	ed4:						std_logic_vector(11 downto 0)	:= x"2c0";
constant	ed5:						std_logic_vector(11 downto 0)	:= x"2c1";
constant	ed6:						std_logic_vector(11 downto 0)	:= x"2c2";
constant	ed7:						std_logic_vector(11 downto 0)	:= x"2c3";
constant	ed8:						std_logic_vector(11 downto 0)	:= x"2c4";
constant	ed9:						std_logic_vector(11 downto 0)	:= x"2c5";
constant	ed10:						std_logic_vector(11 downto 0)	:= x"2c6";

-- 8 bit input to register from a 16 bit port address, 12 cycles, 1 m1 cycle
constant	in16_5:						std_logic_vector(11 downto 0)	:= x"2d0";
constant	in16_6:						std_logic_vector(11 downto 0)	:= x"2d1";

-- 8 bit output to register from a 16 bit port address, 12 cycles, 1 m1 cycle
constant	out16_5:					std_logic_vector(11 downto 0)	:= x"2e0";
constant	out16_6:					std_logic_vector(11 downto 0)	:= x"2e1";

-- 16 bit ADC
constant	adc_sbc_16_5:				std_logic_vector(11 downto 0)	:= x"2f0";

-- store register pair to immediate address
constant	strp16_5:					std_logic_vector(11 downto 0)	:= x"300";
constant	strp16_6:					std_logic_vector(11 downto 0)	:= x"301";

-- load register pair from immediate address
constant	ldrp16_5:					std_logic_vector(11 downto 0)	:= x"310";
constant	ldrp16_6:					std_logic_vector(11 downto 0)	:= x"311";
constant	ldrp16_7:					std_logic_vector(11 downto 0)	:= x"312";

-- NEG
constant	neg6:						std_logic_vector(11 downto 0)	:= x"320";

-- RETN
constant	retn6:						std_logic_vector(11 downto 0)	:= x"330";
constant	retn7:						std_logic_vector(11 downto 0)	:= x"331";
constant	retn8:						std_logic_vector(11 downto 0)	:= x"332";
constant	retn9:						std_logic_vector(11 downto 0)	:= x"333";
constant	retn10:						std_logic_vector(11 downto 0)	:= x"334";
constant	retn11:						std_logic_vector(11 downto 0)	:= x"335";
constant	retn12:						std_logic_vector(11 downto 0)	:= x"336";
constant	retn13:						std_logic_vector(11 downto 0)	:= x"337";
constant	retn14:						std_logic_vector(11 downto 0)	:= x"338";
constant	retn15:						std_logic_vector(11 downto 0)	:= x"339";
constant	retn16:						std_logic_vector(11 downto 0)	:= x"33a";
constant	retn17:						std_logic_vector(11 downto 0)	:= x"33b";
constant	retn18:						std_logic_vector(11 downto 0)	:= x"33c";
constant	retn19:						std_logic_vector(11 downto 0)	:= x"33d";
constant	retn20:						std_logic_vector(11 downto 0)	:= x"33e";
constant	retn21:						std_logic_vector(11 downto 0)	:= x"33f";
constant	retn22:						std_logic_vector(11 downto 0)	:= x"340";
constant	retn23:						std_logic_vector(11 downto 0)	:= x"342";
constant	retn24:						std_logic_vector(11 downto 0)	:= x"343";
constant	retn25:						std_logic_vector(11 downto 0)	:= x"344";

-- RETI
constant	reti6:						std_logic_vector(11 downto 0)	:= x"350";
constant	reti7:						std_logic_vector(11 downto 0)	:= x"351";
constant	reti8:						std_logic_vector(11 downto 0)	:= x"352";
constant	reti9:						std_logic_vector(11 downto 0)	:= x"353";
constant	reti10:						std_logic_vector(11 downto 0)	:= x"354";
constant	reti11:						std_logic_vector(11 downto 0)	:= x"355";
constant	reti12:						std_logic_vector(11 downto 0)	:= x"356";
constant	reti13:						std_logic_vector(11 downto 0)	:= x"357";
constant	reti14:						std_logic_vector(11 downto 0)	:= x"358";
constant	reti15:						std_logic_vector(11 downto 0)	:= x"359";
constant	reti16:						std_logic_vector(11 downto 0)	:= x"35a";
constant	reti17:						std_logic_vector(11 downto 0)	:= x"35b";
constant	reti18:						std_logic_vector(11 downto 0)	:= x"35c";
constant	reti19:						std_logic_vector(11 downto 0)	:= x"35d";
constant	reti20:						std_logic_vector(11 downto 0)	:= x"35e";
constant	reti21:						std_logic_vector(11 downto 0)	:= x"35f";
constant	reti22:						std_logic_vector(11 downto 0)	:= x"360";
constant	reti23:						std_logic_vector(11 downto 0)	:= x"361";
constant	reti24:						std_logic_vector(11 downto 0)	:= x"362";
constant	reti25:						std_logic_vector(11 downto 0)	:= x"363";

-- IM n
constant	im0_6:						std_logic_vector(11 downto 0)	:= x"370";
constant	im0_7:						std_logic_vector(11 downto 0)	:= x"371";
constant	im0_8:						std_logic_vector(11 downto 0)	:= x"372";
constant	im0_9:						std_logic_vector(11 downto 0)	:= x"373";
constant	im0_10:						std_logic_vector(11 downto 0)	:= x"374";
constant	im0_11:						std_logic_vector(11 downto 0)	:= x"375";
constant	im0_12:						std_logic_vector(11 downto 0)	:= x"376";
constant	im0_13:						std_logic_vector(11 downto 0)	:= x"377";

constant	im1_6:						std_logic_vector(11 downto 0)	:= x"380";
constant	im1_7:						std_logic_vector(11 downto 0)	:= x"381";
constant	im1_8:						std_logic_vector(11 downto 0)	:= x"382";
constant	im1_9:						std_logic_vector(11 downto 0)	:= x"383";
constant	im1_10:						std_logic_vector(11 downto 0)	:= x"384";
constant	im1_11:						std_logic_vector(11 downto 0)	:= x"385";
constant	im1_12:						std_logic_vector(11 downto 0)	:= x"386";
constant	im1_13:						std_logic_vector(11 downto 0)	:= x"387";

constant	im2_6:						std_logic_vector(11 downto 0)	:= x"390";
constant	im2_7:						std_logic_vector(11 downto 0)	:= x"391";
constant	im2_8:						std_logic_vector(11 downto 0)	:= x"392";
constant	im2_9:						std_logic_vector(11 downto 0)	:= x"393";
constant	im2_10:						std_logic_vector(11 downto 0)	:= x"394";
constant	im2_11:						std_logic_vector(11 downto 0)	:= x"395";
constant	im2_12:						std_logic_vector(11 downto 0)	:= x"396";
constant	im2_13:						std_logic_vector(11 downto 0)	:= x"397";

-- LD I, A
constant	ldia5:						std_logic_vector(11 downto 0)	:= x"3a0";
constant	ldia6:						std_logic_vector(11 downto 0)	:= x"3a1";
constant	ldia7:						std_logic_vector(11 downto 0)	:= x"3a2";
constant	ldia8:						std_logic_vector(11 downto 0)	:= x"3a3";
constant	ldia9:						std_logic_vector(11 downto 0)	:= x"3a4";
constant	ldia10:						std_logic_vector(11 downto 0)	:= x"3a5";
constant	ldia11:						std_logic_vector(11 downto 0)	:= x"3a6";
constant	ldia12:						std_logic_vector(11 downto 0)	:= x"3a7";
constant	ldia13:						std_logic_vector(11 downto 0)	:= x"3a8";
constant	ldia14:						std_logic_vector(11 downto 0)	:= x"3a9";

-- LD R, A, ignore this instruction

-- LD A, I
constant	ldai5:						std_logic_vector(11 downto 0)	:= x"3b0";
constant	ldai6:						std_logic_vector(11 downto 0)	:= x"3b1";
constant	ldai7:						std_logic_vector(11 downto 0)	:= x"3b2";
constant	ldai8:						std_logic_vector(11 downto 0)	:= x"3b3";
constant	ldai9:						std_logic_vector(11 downto 0)	:= x"3b4";
constant	ldai10:						std_logic_vector(11 downto 0)	:= x"3b5";
constant	ldai11:						std_logic_vector(11 downto 0)	:= x"3b6";
constant	ldai12:						std_logic_vector(11 downto 0)	:= x"3b7";
constant	ldai13:						std_logic_vector(11 downto 0)	:= x"3b8";
constant	ldai14:						std_logic_vector(11 downto 0)	:= x"3b9";

-- LD A, R, ignore this instruction

-- RRD and RLD
constant	rrd_rld5:					std_logic_vector(11 downto 0)	:= x"3c0";

-- Block instructions

-- LDI
constant	bldi5:						std_logic_vector(11 downto 0)	:= x"3d0";
constant	bldi6:						std_logic_vector(11 downto 0)	:= x"3d1";
constant	bldi7:						std_logic_vector(11 downto 0)	:= x"3d2";
constant	bldi8:						std_logic_vector(11 downto 0)	:= x"3d3";
constant	bldi9:						std_logic_vector(11 downto 0)	:= x"3d4";
constant	bldi10:						std_logic_vector(11 downto 0)	:= x"3d5";
constant	bldi11:						std_logic_vector(11 downto 0)	:= x"3d6";
constant	bldi12:						std_logic_vector(11 downto 0)	:= x"3d7";
constant	bldi13:						std_logic_vector(11 downto 0)	:= x"3d8";
constant	bldi14:						std_logic_vector(11 downto 0)	:= x"3d9";

-- CPI
constant	bcpi5:						std_logic_vector(11 downto 0)	:= x"3e0";
constant	bcpi6:						std_logic_vector(11 downto 0)	:= x"3e1";
constant	bcpi7:						std_logic_vector(11 downto 0)	:= x"3e2";
constant	bcpi8:						std_logic_vector(11 downto 0)	:= x"3e3";
constant	bcpi9:						std_logic_vector(11 downto 0)	:= x"3e4";
constant	bcpi10:						std_logic_vector(11 downto 0)	:= x"3e5";
constant	bcpi11:						std_logic_vector(11 downto 0)	:= x"3e6";

-- INI
constant	bini5:						std_logic_vector(11 downto 0)	:= x"3f0";
constant	bini6:						std_logic_vector(11 downto 0)	:= x"3f1";
constant	bini7:						std_logic_vector(11 downto 0)	:= x"3f2";
constant	bini8:						std_logic_vector(11 downto 0)	:= x"3f3";
constant	bini9:						std_logic_vector(11 downto 0)	:= x"3f4";
constant	bini10:						std_logic_vector(11 downto 0)	:= x"3f5";
constant	bini11:						std_logic_vector(11 downto 0)	:= x"3f6";

-- OUTI
constant	bouti5:						std_logic_vector(11 downto 0)	:= x"400";
constant	bouti6:						std_logic_vector(11 downto 0)	:= x"401";
constant	bouti7:						std_logic_vector(11 downto 0)	:= x"402";
constant	bouti8:						std_logic_vector(11 downto 0)	:= x"403";
constant	bouti9:						std_logic_vector(11 downto 0)	:= x"404";
constant	bouti10:					std_logic_vector(11 downto 0)	:= x"405";
constant	bouti11:					std_logic_vector(11 downto 0)	:= x"406";
constant	bouti12:					std_logic_vector(11 downto 0)	:= x"407";

-- LDD
constant	bldd5:						std_logic_vector(11 downto 0)	:= x"410";
constant	bldd6:						std_logic_vector(11 downto 0)	:= x"411";
constant	bldd7:						std_logic_vector(11 downto 0)	:= x"412";
constant	bldd8:						std_logic_vector(11 downto 0)	:= x"413";
constant	bldd9:						std_logic_vector(11 downto 0)	:= x"414";
constant	bldd10:						std_logic_vector(11 downto 0)	:= x"415";
constant	bldd11:						std_logic_vector(11 downto 0)	:= x"416";
constant	bldd12:						std_logic_vector(11 downto 0)	:= x"417";
constant	bldd13:						std_logic_vector(11 downto 0)	:= x"418";

-- CPD
constant	bcpd5:						std_logic_vector(11 downto 0)	:= x"420";
constant	bcpd6:						std_logic_vector(11 downto 0)	:= x"421";
constant	bcpd7:						std_logic_vector(11 downto 0)	:= x"422";
constant	bcpd8:						std_logic_vector(11 downto 0)	:= x"423";
constant	bcpd9:						std_logic_vector(11 downto 0)	:= x"424";
constant	bcpd10:						std_logic_vector(11 downto 0)	:= x"425";
constant	bcpd11:						std_logic_vector(11 downto 0)	:= x"426";

-- IND
constant	bind5:						std_logic_vector(11 downto 0)	:= x"430";
constant	bind6:						std_logic_vector(11 downto 0)	:= x"431";
constant	bind7:						std_logic_vector(11 downto 0)	:= x"432";
constant	bind8:						std_logic_vector(11 downto 0)	:= x"433";
constant	bind9:						std_logic_vector(11 downto 0)	:= x"434";
constant	bind10:						std_logic_vector(11 downto 0)	:= x"435";
constant	bind11:						std_logic_vector(11 downto 0)	:= x"436";

-- OUTD
constant	boutd5:						std_logic_vector(11 downto 0)	:= x"440";
constant	boutd6:						std_logic_vector(11 downto 0)	:= x"441";
constant	boutd7:						std_logic_vector(11 downto 0)	:= x"442";
constant	boutd8:						std_logic_vector(11 downto 0)	:= x"443";
constant	boutd9:						std_logic_vector(11 downto 0)	:= x"444";
constant	boutd10:					std_logic_vector(11 downto 0)	:= x"445";
constant	boutd11:					std_logic_vector(11 downto 0)	:= x"446";

-- LDIR
constant	bldir5:						std_logic_vector(11 downto 0)	:= x"450";
constant	bldir6:						std_logic_vector(11 downto 0)	:= x"451";
--constant	bldir7:						std_logic_vector(11 downto 0)	:= x"452";
--constant	bldir8:						std_logic_vector(11 downto 0)	:= x"453";
--constant	bldir9:			  			std_logic_vector(11 downto 0)	:= x"454";
--constant	bldir10:					std_logic_vector(11 downto 0)	:= x"455";
--constant	bldir11:					std_logic_vector(11 downto 0)	:= x"456";
--constant	bldir12:					std_logic_vector(11 downto 0)	:= x"457";
--constant	bldir13:					std_logic_vector(11 downto 0)	:= x"458";
--constant	bldir14:					std_logic_vector(11 downto 0)	:= x"459";
--constant	bldir15:					std_logic_vector(11 downto 0)	:= x"45a";
--constant	bldir16:					std_logic_vector(11 downto 0)	:= x"45b";
--constant	bldir17:					std_logic_vector(11 downto 0)	:= x"45c";
--constant	bldir18:					std_logic_vector(11 downto 0)	:= x"45d";
--constant	bldir19:					std_logic_vector(11 downto 0)	:= x"45e";
--constant	bldir20:					std_logic_vector(11 downto 0)	:= x"45f";
--constant	bldir21:					std_logic_vector(11 downto 0)	:= x"460";
--constant	bldir22:					std_logic_vector(11 downto 0)	:= x"461";
--constant	bldir23:					std_logic_vector(11 downto 0)	:= x"462";
--constant	bldir24:					std_logic_vector(11 downto 0)	:= x"463";
--constant	bldir25:					std_logic_vector(11 downto 0)	:= x"464";
--constant	bldir26:					std_logic_vector(11 downto 0)	:= x"465";
--constant	bldir27:					std_logic_vector(11 downto 0)	:= x"466";
--constant	bldir28:					std_logic_vector(11 downto 0)	:= x"467";
--constant	bldir29:					std_logic_vector(11 downto 0)	:= x"468";
--constant	bldir30:					std_logic_vector(11 downto 0)	:= x"469";
--constant	bldir31:					std_logic_vector(11 downto 0)	:= x"46a";
--constant	bldir32:					std_logic_vector(11 downto 0)	:= x"46b";
--constant	bldir33:					std_logic_vector(11 downto 0)	:= x"46c";
--constant	bldir34:					std_logic_vector(11 downto 0)	:= x"46d";
--constant	bldir35:					std_logic_vector(11 downto 0)	:= x"46e";
--constant	bldir36:					std_logic_vector(11 downto 0)	:= x"46f";
--constant	bldir37:					std_logic_vector(11 downto 0)	:= x"470";
--constant	bldir38:					std_logic_vector(11 downto 0)	:= x"471";

-- CPIR
constant	bcpir5:						std_logic_vector(11 downto 0)	:= x"480";
constant	bcpir6:						std_logic_vector(11 downto 0)	:= x"481";
--constant	bcpir7:						std_logic_vector(11 downto 0)	:= x"482";
--constant	bcpir8:						std_logic_vector(11 downto 0)	:= x"483";
--constant	bcpir9:						std_logic_vector(11 downto 0)	:= x"484";
--constant	bcpir10:					std_logic_vector(11 downto 0)	:= x"485";
--constant	bcpir11:					std_logic_vector(11 downto 0)	:= x"486";
--constant	bcpir12:					std_logic_vector(11 downto 0)	:= x"487";
--constant	bcpir13:					std_logic_vector(11 downto 0)	:= x"488";
--constant	bcpir14:					std_logic_vector(11 downto 0)	:= x"489";
--constant	bcpir15:					std_logic_vector(11 downto 0)	:= x"48a";
--constant	bcpir16:					std_logic_vector(11 downto 0)	:= x"48b";
--constant	bcpir17:					std_logic_vector(11 downto 0)	:= x"48c";
--constant	bcpir18:					std_logic_vector(11 downto 0)	:= x"48d";
--constant	bcpir19:					std_logic_vector(11 downto 0)	:= x"48e";
--constant	bcpir20:					std_logic_vector(11 downto 0)	:= x"48f";
--constant	bcpir21:					std_logic_vector(11 downto 0)	:= x"490";
--constant	bcpir22:					std_logic_vector(11 downto 0)	:= x"491";
--constant	bcpir23:					std_logic_vector(11 downto 0)	:= x"492";
--constant	bcpir24:					std_logic_vector(11 downto 0)	:= x"493";
--constant	bcpir25:					std_logic_vector(11 downto 0)	:= x"494";
--constant	bcpir26:					std_logic_vector(11 downto 0)	:= x"495";
--constant	bcpir27:					std_logic_vector(11 downto 0)	:= x"496";
--constant	bcpir28:					std_logic_vector(11 downto 0)	:= x"497";
--constant	bcpir29:					std_logic_vector(11 downto 0)	:= x"498";
--constant	bcpir30:					std_logic_vector(11 downto 0)	:= x"499";
--constant	bcpir31:					std_logic_vector(11 downto 0)	:= x"49a";
--constant	bcpir32:					std_logic_vector(11 downto 0)	:= x"49b";
--constant	bcpir33:					std_logic_vector(11 downto 0)	:= x"49c";
--constant	bcpir34:					std_logic_vector(11 downto 0)	:= x"49d";
--constant	bcpir35:					std_logic_vector(11 downto 0)	:= x"49e";
--constant	bcpir36:					std_logic_vector(11 downto 0)	:= x"49f";
--constant	bcpir37:					std_logic_vector(11 downto 0)	:= x"4a0";
--constant	bcpir38:					std_logic_vector(11 downto 0)	:= x"4a1";

-- INIR
constant	binir5:						std_logic_vector(11 downto 0)	:= x"4b0";
constant	binir6:						std_logic_vector(11 downto 0)	:= x"4b1";
--constant	binir7:						std_logic_vector(11 downto 0)	:= x"4b2";
--constant	binir8:						std_logic_vector(11 downto 0)	:= x"4b3";
--constant	binir9:						std_logic_vector(11 downto 0)	:= x"4b4";
--constant	binir10:					std_logic_vector(11 downto 0)	:= x"4b5";
--constant	binir11:					std_logic_vector(11 downto 0)	:= x"4b6";
--constant	binir12:					std_logic_vector(11 downto 0)	:= x"4b7";
--constant	binir13:					std_logic_vector(11 downto 0)	:= x"4b8";
--constant	binir14:					std_logic_vector(11 downto 0)	:= x"4b9";
--constant	binir15:					std_logic_vector(11 downto 0)	:= x"4ba";
--constant	binir16:					std_logic_vector(11 downto 0)	:= x"4bb";
--constant	binir17:					std_logic_vector(11 downto 0)	:= x"4bc";
--constant	binir18:					std_logic_vector(11 downto 0)	:= x"4bd";
--constant	binir19:					std_logic_vector(11 downto 0)	:= x"4be";
--constant	binir20:					std_logic_vector(11 downto 0)	:= x"4bf";
--constant	binir21:					std_logic_vector(11 downto 0)	:= x"4c0";
--constant	binir22:					std_logic_vector(11 downto 0)	:= x"4c1";
--constant	binir23:					std_logic_vector(11 downto 0)	:= x"4c2";
--constant	binir24:					std_logic_vector(11 downto 0)	:= x"4c3";
--constant	binir25:					std_logic_vector(11 downto 0)	:= x"4c4";
--constant	binir26:					std_logic_vector(11 downto 0)	:= x"4c5";
--constant	binir27:					std_logic_vector(11 downto 0)	:= x"4c6";
--constant	binir28:					std_logic_vector(11 downto 0)	:= x"4c7";
--constant	binir29:					std_logic_vector(11 downto 0)	:= x"4c8";
--constant	binir30:					std_logic_vector(11 downto 0)	:= x"4c9";
--constant	binir31:					std_logic_vector(11 downto 0)	:= x"4ca";
--constant	binir32:					std_logic_vector(11 downto 0)	:= x"4cb";
--constant	binir33:					std_logic_vector(11 downto 0)	:= x"4cc";
--constant	binir34:					std_logic_vector(11 downto 0)	:= x"4cd";
--constant	binir35:					std_logic_vector(11 downto 0)	:= x"4ce";
--constant	binir36:					std_logic_vector(11 downto 0)	:= x"4cf";
--constant	binir37:					std_logic_vector(11 downto 0)	:= x"4d0";
--constant	binir38:					std_logic_vector(11 downto 0)	:= x"4d1";

-- OTIR
constant	botir5:						std_logic_vector(11 downto 0)	:= x"4e0";
constant	botir6:						std_logic_vector(11 downto 0)	:= x"4e1";
--constant	botir7:						std_logic_vector(11 downto 0)	:= x"4e2";
--constant	botir8:						std_logic_vector(11 downto 0)	:= x"4e3";
--constant	botir9:						std_logic_vector(11 downto 0)	:= x"4e4";
--constant	botir10:					std_logic_vector(11 downto 0)	:= x"4e5";
--constant	botir11:					std_logic_vector(11 downto 0)	:= x"4e6";
--constant	botir12:					std_logic_vector(11 downto 0)	:= x"4e7";
--constant	botir13:					std_logic_vector(11 downto 0)	:= x"4e8";
--constant	botir14:					std_logic_vector(11 downto 0)	:= x"4e9";
--constant	botir15:					std_logic_vector(11 downto 0)	:= x"4ea";
--constant	botir16:					std_logic_vector(11 downto 0)	:= x"4eb";
--constant	botir17:					std_logic_vector(11 downto 0)	:= x"4ec";
--constant	botir18:					std_logic_vector(11 downto 0)	:= x"4ed";
--constant	botir19:					std_logic_vector(11 downto 0)	:= x"4ee";
--constant	botir20:					std_logic_vector(11 downto 0)	:= x"4ef";
--constant	botir21:					std_logic_vector(11 downto 0)	:= x"4f0";
--constant	botir22:					std_logic_vector(11 downto 0)	:= x"4f1";
--constant	botir23:					std_logic_vector(11 downto 0)	:= x"4f2";
--constant	botir24:					std_logic_vector(11 downto 0)	:= x"4f3";
--constant	botir25:					std_logic_vector(11 downto 0)	:= x"4f4";
--constant	botir26:					std_logic_vector(11 downto 0)	:= x"4f5";
--constant	botir27:					std_logic_vector(11 downto 0)	:= x"4f6";
--constant	botir28:					std_logic_vector(11 downto 0)	:= x"4f7";
--constant	botir29:					std_logic_vector(11 downto 0)	:= x"4f8";
--constant	botir30:					std_logic_vector(11 downto 0)	:= x"4f9";
--constant	botir31:					std_logic_vector(11 downto 0)	:= x"4fa";
--constant	botir32:					std_logic_vector(11 downto 0)	:= x"4fb";
--constant	botir33:					std_logic_vector(11 downto 0)	:= x"4fc";
--constant	botir34:					std_logic_vector(11 downto 0)	:= x"4fd";
--constant	botir35:					std_logic_vector(11 downto 0)	:= x"4fe";
--constant	botir36:					std_logic_vector(11 downto 0)	:= x"4ff";
--constant	botir37:					std_logic_vector(11 downto 0)	:= x"500";
--constant	botir38:					std_logic_vector(11 downto 0)	:= x"501";

-- LDDR
constant	blddr5:						std_logic_vector(11 downto 0)	:= x"510";
constant	blddr6:						std_logic_vector(11 downto 0)	:= x"511";
--constant	blddr7:						std_logic_vector(11 downto 0)	:= x"512";
--constant	blddr8:						std_logic_vector(11 downto 0)	:= x"513";
--constant	blddr9:						std_logic_vector(11 downto 0)	:= x"514";
--constant	blddr10:					std_logic_vector(11 downto 0)	:= x"515";
--constant	blddr11:					std_logic_vector(11 downto 0)	:= x"516";
--constant	blddr12:					std_logic_vector(11 downto 0)	:= x"517";
--constant	blddr13:					std_logic_vector(11 downto 0)	:= x"518";
--constant	blddr14:					std_logic_vector(11 downto 0)	:= x"519";
--constant	blddr15:					std_logic_vector(11 downto 0)	:= x"51a";
--constant	blddr16:					std_logic_vector(11 downto 0)	:= x"51b";
--constant	blddr17:					std_logic_vector(11 downto 0)	:= x"51c";
--constant	blddr18:					std_logic_vector(11 downto 0)	:= x"51d";
--constant	blddr19:					std_logic_vector(11 downto 0)	:= x"51e";
--constant	blddr20:					std_logic_vector(11 downto 0)	:= x"51f";
--constant	blddr21:					std_logic_vector(11 downto 0)	:= x"520";
--constant	blddr22:					std_logic_vector(11 downto 0)	:= x"521";
--constant	blddr23:					std_logic_vector(11 downto 0)	:= x"522";
--constant	blddr24:					std_logic_vector(11 downto 0)	:= x"523";
--constant	blddr25:					std_logic_vector(11 downto 0)	:= x"524";
--constant	blddr26:					std_logic_vector(11 downto 0)	:= x"525";
--constant	blddr27:					std_logic_vector(11 downto 0)	:= x"526";
--constant	blddr28:					std_logic_vector(11 downto 0)	:= x"527";
--constant	blddr29:					std_logic_vector(11 downto 0)	:= x"528";
--constant	blddr30:					std_logic_vector(11 downto 0)	:= x"529";
--constant	blddr31:					std_logic_vector(11 downto 0)	:= x"52a";
--constant	blddr32:					std_logic_vector(11 downto 0)	:= x"52b";
--constant	blddr33:					std_logic_vector(11 downto 0)	:= x"52c";
--constant	blddr34:					std_logic_vector(11 downto 0)	:= x"52d";
--constant	blddr35:					std_logic_vector(11 downto 0)	:= x"52e";
--constant	blddr36:					std_logic_vector(11 downto 0)	:= x"52f";
--constant	blddr37:					std_logic_vector(11 downto 0)	:= x"530";
--constant	blddr38:					std_logic_vector(11 downto 0)	:= x"531";

-- CPDR
constant	bcpdr5:						std_logic_vector(11 downto 0)	:= x"540";
constant	bcpdr6:						std_logic_vector(11 downto 0)	:= x"541";
--constant	bcpdr7:						std_logic_vector(11 downto 0)	:= x"542";
--constant	bcpdr8:						std_logic_vector(11 downto 0)	:= x"543";
--constant	bcpdr9:						std_logic_vector(11 downto 0)	:= x"544";
--constant	bcpdr10:					std_logic_vector(11 downto 0)	:= x"545";
--constant	bcpdr11:					std_logic_vector(11 downto 0)	:= x"546";
--constant	bcpdr12:					std_logic_vector(11 downto 0)	:= x"547";
--constant	bcpdr13:					std_logic_vector(11 downto 0)	:= x"548";
--constant	bcpdr14:					std_logic_vector(11 downto 0)	:= x"549";
--constant	bcpdr15:					std_logic_vector(11 downto 0)	:= x"54a";
--constant	bcpdr16:					std_logic_vector(11 downto 0)	:= x"54b";
--constant	bcpdr17:					std_logic_vector(11 downto 0)	:= x"54c";
--constant	bcpdr18:					std_logic_vector(11 downto 0)	:= x"54d";
--constant	bcpdr19:					std_logic_vector(11 downto 0)	:= x"54e";
--constant	bcpdr20:					std_logic_vector(11 downto 0)	:= x"54f";
--constant	bcpdr21:					std_logic_vector(11 downto 0)	:= x"550";
--constant	bcpdr22:					std_logic_vector(11 downto 0)	:= x"551";
--constant	bcpdr23:					std_logic_vector(11 downto 0)	:= x"552";
--constant	bcpdr24:					std_logic_vector(11 downto 0)	:= x"553";
--constant	bcpdr25:					std_logic_vector(11 downto 0)	:= x"554";
--constant	bcpdr26:					std_logic_vector(11 downto 0)	:= x"555";
--constant	bcpdr27:					std_logic_vector(11 downto 0)	:= x"556";
--constant	bcpdr28:					std_logic_vector(11 downto 0)	:= x"557";
--constant	bcpdr29:					std_logic_vector(11 downto 0)	:= x"558";
--constant	bcpdr30:					std_logic_vector(11 downto 0)	:= x"559";
--constant	bcpdr32:					std_logic_vector(11 downto 0)	:= x"55a";
--constant	bcpdr33:					std_logic_vector(11 downto 0)	:= x"55b";
--constant	bcpdr34:					std_logic_vector(11 downto 0)	:= x"55c";
--constant	bcpdr35:					std_logic_vector(11 downto 0)	:= x"55d";
--constant	bcpdr36:					std_logic_vector(11 downto 0)	:= x"55e";
--constant	bcpdr37:					std_logic_vector(11 downto 0)	:= x"55f";
--constant	bcpdr38:					std_logic_vector(11 downto 0)	:= x"560";
--constant	bcpdr39:					std_logic_vector(11 downto 0)	:= x"561";

-- INDR
constant	bindr5:						std_logic_vector(11 downto 0)	:= x"570";
constant	bindr6:						std_logic_vector(11 downto 0)	:= x"571";
--constant	bindr7:						std_logic_vector(11 downto 0)	:= x"572";
--constant	bindr8:						std_logic_vector(11 downto 0)	:= x"573";
--constant	bindr9:						std_logic_vector(11 downto 0)	:= x"574";
--constant	bindr13:					std_logic_vector(11 downto 0)	:= x"575";
--constant	bindr10:					std_logic_vector(11 downto 0)	:= x"576";
--constant	bindr18:					std_logic_vector(11 downto 0)	:= x"577";
--constant	bindr12:					std_logic_vector(11 downto 0)	:= x"578";
--constant	bindr13:					std_logic_vector(11 downto 0)	:= x"579";
--constant	bindr14:					std_logic_vector(11 downto 0)	:= x"57a";
--constant	bindr15:					std_logic_vector(11 downto 0)	:= x"57b";
--constant	bindr16:					std_logic_vector(11 downto 0)	:= x"57c";
--constant	bindr17:					std_logic_vector(11 downto 0)	:= x"57d";
--constant	bindr18:					std_logic_vector(11 downto 0)	:= x"57e";
--constant	bindr19:					std_logic_vector(11 downto 0)	:= x"57f";
--constant	bindr20:					std_logic_vector(11 downto 0)	:= x"580";
--constant	bindr21:					std_logic_vector(11 downto 0)	:= x"581";
--constant	bindr22:					std_logic_vector(11 downto 0)	:= x"582";
--constant	bindr23:					std_logic_vector(11 downto 0)	:= x"583";
--constant	bindr24:					std_logic_vector(11 downto 0)	:= x"584";
--constant	bindr25:					std_logic_vector(11 downto 0)	:= x"585";
--constant	bindr26:					std_logic_vector(11 downto 0)	:= x"586";
--constant	bindr27:					std_logic_vector(11 downto 0)	:= x"587";
--constant	bindr28:					std_logic_vector(11 downto 0)	:= x"588";
--constant	bindr29:					std_logic_vector(11 downto 0)	:= x"589";
--constant	bindr30:					std_logic_vector(11 downto 0)	:= x"58a";
--constant	bindr31:					std_logic_vector(11 downto 0)	:= x"58b";
--constant	bindr32:					std_logic_vector(11 downto 0)	:= x"58c";
--constant	bindr33:					std_logic_vector(11 downto 0)	:= x"58d";
--constant	bindr34:					std_logic_vector(11 downto 0)	:= x"58e";
--constant	bindr35:					std_logic_vector(11 downto 0)	:= x"58f";
--constant	bindr36:					std_logic_vector(11 downto 0)	:= x"590";
--constant	bindr37:					std_logic_vector(11 downto 0)	:= x"591";

-- OTDR
constant	botdr5:						std_logic_vector(11 downto 0)	:= x"5a0";
constant	botdr6:						std_logic_vector(11 downto 0)	:= x"5a1";
--constant	botdr7:						std_logic_vector(11 downto 0)	:= x"5a2";
--constant	botdr8:						std_logic_vector(11 downto 0)	:= x"5a3";
--constant	botdr9:						std_logic_vector(11 downto 0)	:= x"5a4";
--constant	botdr10:					std_logic_vector(11 downto 0)	:= x"5a5";
--constant	botdr11:					std_logic_vector(11 downto 0)	:= x"5a6";
--constant	botdr12:					std_logic_vector(11 downto 0)	:= x"5a7";
--constant	botdr13:					std_logic_vector(11 downto 0)	:= x"5a8";
--constant	botdr14:					std_logic_vector(11 downto 0)	:= x"5a9";
--constant	botdr15:					std_logic_vector(11 downto 0)	:= x"5aa";
--constant	botdr16:					std_logic_vector(11 downto 0)	:= x"5ab";
--constant	botdr17:					std_logic_vector(11 downto 0)	:= x"5ac";
--constant	botdr18:					std_logic_vector(11 downto 0)	:= x"5ad";
--constant	botdr19:					std_logic_vector(11 downto 0)	:= x"5ae";
--constant	botdr20:					std_logic_vector(11 downto 0)	:= x"5af";
--constant	botdr21:					std_logic_vector(11 downto 0)	:= x"5b0";
--constant	botdr22:					std_logic_vector(11 downto 0)	:= x"5b1";
--constant	botdr23:					std_logic_vector(11 downto 0)	:= x"5b2";
--constant	botdr24:					std_logic_vector(11 downto 0)	:= x"5b3";
--constant	botdr25:					std_logic_vector(11 downto 0)	:= x"5b4";
--constant	botdr26:					std_logic_vector(11 downto 0)	:= x"5b5";
--constant	botdr27:					std_logic_vector(11 downto 0)	:= x"5b6";
--constant	botdr28:					std_logic_vector(11 downto 0)	:= x"5b7";
--constant	botdr29:					std_logic_vector(11 downto 0)	:= x"5b8";
--constant	botdr30:					std_logic_vector(11 downto 0)	:= x"5b9";
--constant	botdr31:					std_logic_vector(11 downto 0)	:= x"5ba";
--constant	botdr32:					std_logic_vector(11 downto 0)	:= x"5bb";
--constant	botdr33:					std_logic_vector(11 downto 0)	:= x"5bc";
--constant	botdr34:					std_logic_vector(11 downto 0)	:= x"5bd";
--constant	botdr35:					std_logic_vector(11 downto 0)	:= x"5be";
--constant	botdr36:					std_logic_vector(11 downto 0)	:= x"5bf";
--constant	botdr37:					std_logic_vector(11 downto 0)	:= x"5c0";
--constant	botdr38:					std_logic_vector(11 downto 0)	:= x"5c1";

-- end of ED prefixed opcodes

-- index register instructions
constant	index4:						std_logic_vector(11 downto 0)	:= x"5d0";
constant	index5:						std_logic_vector(11 downto 0)	:= x"5d1";
constant	index6:						std_logic_vector(11 downto 0)	:= x"5d2";
constant	index7:						std_logic_vector(11 downto 0)	:= x"5d3";
constant	index8:						std_logic_vector(11 downto 0)	:= x"5d4";

-- alu ops on immediate operand, 7 cycles, 1 m1 cycle
constant	alui4:						std_logic_vector(11 downto 0)	:= x"5e0";
constant	alui5:						std_logic_vector(11 downto 0)	:= x"5e1";

-- RST, 11 clock cycles, 1 m1 cycle
constant	rst4:						std_logic_vector(11 downto 0)	:= x"5f0";
constant	rst5:						std_logic_vector(11 downto 0)	:= x"5f1";
constant	rst6:						std_logic_vector(11 downto 0)	:= x"5f2";
constant	rst7:						std_logic_vector(11 downto 0)	:= x"5f3";
constant	rst8:						std_logic_vector(11 downto 0)	:= x"5f4";

-- get next opcode byte
constant	nxtop1:						std_logic_vector(11 downto 0)	:= x"600";
constant	nxtop2:						std_logic_vector(11 downto 0)	:= x"601";

-- get next operand byte
constant	nxtoprnd1:					std_logic_vector(11 downto 0)	:= x"610";
constant	nxtoprnd2:					std_logic_vector(11 downto 0)	:= x"611";

-- general memory read
constant	genmemrd1:					std_logic_vector(11 downto 0)	:= x"620";
constant	genmemrd2:					std_logic_vector(11 downto 0)	:= x"621";
constant	genmemrd3:					std_logic_vector(11 downto 0)	:= x"622";

-- general memory write
constant	genmemwrt1:					std_logic_vector(11 downto 0)	:= x"630";
constant	genmemwrt2:					std_logic_vector(11 downto 0)	:= x"631";
constant	genmemwrt3:					std_logic_vector(11 downto 0)	:= x"632";
constant	genmemwrt4:					std_logic_vector(11 downto 0)	:= x"633";

-- for 2-byte operands
constant	obtain_2byte_operand1:		std_logic_vector(11 downto 0)	:= x"640";
constant	obtain_2byte_operand2:		std_logic_vector(11 downto 0)	:= x"641";

-- for SP increment
constant	incSP1:						std_logic_vector(11 downto 0)	:= x"650";

-- for SP decrement
constant	decSP1:						std_logic_vector(11 downto 0)	:= x"660";

-- for handling non-maskable interrupts
constant	nmi1:						std_logic_vector(11 downto 0)	:= x"670";

-- for handling maskable interrupts
constant	int1:						std_logic_vector(11 downto 0)	:= x"680";

-- index register bit operations
constant	index_bit5:					std_logic_vector(11 downto 0)	:= x"690";
constant	index_bit6:					std_logic_vector(11 downto 0)	:= x"691";
constant	index_bit7:					std_logic_vector(11 downto 0)	:= x"692";

-- BIT
constant	index_bit_bit8:				std_logic_vector(11 downto 0)	:= x"6a0";

-- common state for saving index bit operation results other than BIT
constant	index_save8:				std_logic_vector(11 downto 0)	:= x"6b0";

-- load index register with immediate operand
constant	ld_index_immediate5:		std_logic_vector(11 downto 0)	:= x"6c0";
constant	ld_index_immediate6:		std_logic_vector(11 downto 0)	:= x"6c1";

-- add index register to register pair
constant	add_index_rp5:	   			std_logic_vector(11 downto 0)	:= x"6d0";
constant	add_index_rp6:  			std_logic_vector(11 downto 0)	:= x"6d1";

-- store index register direct
constant	st_index_direct5:			std_logic_vector(11 downto 0)	:= x"6e0";
constant	st_index_direct6:			std_logic_vector(11 downto 0)	:= x"6e1";

-- load index register direct
constant	ld_index_direct5:			std_logic_vector(11 downto 0)	:= x"6f0";
constant	ld_index_direct6:			std_logic_vector(11 downto 0)	:= x"6f1";
constant	ld_index_direct7:			std_logic_vector(11 downto 0)	:= x"6f2";
constant	ld_index_direct8:			std_logic_vector(11 downto 0)	:= x"6f3";
constant	ld_index_direct9:			std_logic_vector(11 downto 0)	:= x"6f4";
constant	ld_index_direct10:			std_logic_vector(11 downto 0)	:= x"6f5";
constant	ld_index_direct11:			std_logic_vector(11 downto 0)	:= x"6f6";
constant	ld_index_direct12:			std_logic_vector(11 downto 0)	:= x"6f7";
constant	ld_index_direct13:			std_logic_vector(11 downto 0)	:= x"6f8";
constant	ld_index_direct14:			std_logic_vector(11 downto 0)	:= x"6f9";

-- increment or decrement index register
constant	incdec_index5: 				std_logic_vector(11 downto 0)	:= x"700";
constant	incdec_index6:				std_logic_vector(11 downto 0)	:= x"701";

-- increment or decrement memory at index register + offset
constant	incdec_index_memory5: 		std_logic_vector(11 downto 0)	:= x"710";
constant	incdec_index_memory6:		std_logic_vector(11 downto 0)	:= x"711";

-- load immediate to index register + offset
constant	ld_index_memory_immed5:		std_logic_vector(11 downto 0)	:= x"720";
constant	ld_index_memory_immed6:		std_logic_vector(11 downto 0)	:= x"721";

-- store 8 bit register to index register + offset
constant	st_index_memory5:			std_logic_vector(11 downto 0)	:= x"730";

-- load 8 bit register from index register + offset
constant	ld_index_memory5:			std_logic_vector(11 downto 0)	:= x"740";
constant	ld_index_memory6:			std_logic_vector(11 downto 0)	:= x"741";
constant	ld_index_memory7:			std_logic_vector(11 downto 0)	:= x"742";
constant	ld_index_memory8:			std_logic_vector(11 downto 0)	:= x"743";
constant	ld_index_memory9:			std_logic_vector(11 downto 0)	:= x"744";
constant	ld_index_memory10:			std_logic_vector(11 downto 0)	:= x"745";
constant	ld_index_memory11:			std_logic_vector(11 downto 0)	:= x"746";
constant	ld_index_memory12:			std_logic_vector(11 downto 0)	:= x"747";
constant	ld_index_memory13:			std_logic_vector(11 downto 0)	:= x"748";
constant	ld_index_memory14:			std_logic_vector(11 downto 0)	:= x"749";

-- 8 bit ALU operations involving memory pointed to by index register	+ offset
constant	index_alu_ops5:				std_logic_vector(11 downto 0)	:= x"750";
constant	index_alu_ops6:	 			std_logic_vector(11 downto 0)	:= x"751";
constant	index_alu_ops7:	 			std_logic_vector(11 downto 0)	:= x"752";
constant	index_alu_ops8:				std_logic_vector(11 downto 0)	:= x"753";
constant	index_alu_ops9:				std_logic_vector(11 downto 0)	:= x"754";
constant	index_alu_ops10:			std_logic_vector(11 downto 0)	:= x"755";
constant	index_alu_ops11:			std_logic_vector(11 downto 0)	:= x"756";
constant	index_alu_ops12:			std_logic_vector(11 downto 0)	:= x"757";
constant	index_alu_ops13:			std_logic_vector(11 downto 0)	:= x"758";
constant	index_alu_ops14:			std_logic_vector(11 downto 0)	:= x"759";

-- pop index register off stack
constant	pop_index5:	  				std_logic_vector(11 downto 0)	:= x"770";
constant	pop_index6:	 				std_logic_vector(11 downto 0)	:= x"771";
constant	pop_index7:	 				std_logic_vector(11 downto 0)	:= x"772";

-- push index register on stack
constant	push_index5:	  			std_logic_vector(11 downto 0)	:= x"780";
constant	push_index6:	 			std_logic_vector(11 downto 0)	:= x"781";
constant	push_index7:	 			std_logic_vector(11 downto 0)	:= x"782";
constant	push_index8:				std_logic_vector(11 downto 0)	:= x"783";
constant	push_index9:				std_logic_vector(11 downto 0)	:= x"784";
constant	push_index10:				std_logic_vector(11 downto 0)	:= x"785";
constant	push_index11:				std_logic_vector(11 downto 0)	:= x"786";
constant	push_index12:				std_logic_vector(11 downto 0)	:= x"787";
constant	push_index13:				std_logic_vector(11 downto 0)	:= x"788";
constant	push_index14:				std_logic_vector(11 downto 0)	:= x"789";

-- SPI?
constant	sp_index5:	  				std_logic_vector(11 downto 0)	:= x"790";

-- XTI?
constant	xtindex5:	  				std_logic_vector(11 downto 0)	:= x"7a0";
constant	xtindex6:	 				std_logic_vector(11 downto 0)	:= x"7a1";
constant	xtindex7:	 				std_logic_vector(11 downto 0)	:= x"7a2";
constant	xtindex8:					std_logic_vector(11 downto 0)	:= x"7a3";
constant	xtindex9:					std_logic_vector(11 downto 0)	:= x"7a4";
constant	xtindex10:					std_logic_vector(11 downto 0)	:= x"7a5";
constant	xtindex11:					std_logic_vector(11 downto 0)	:= x"7a6";
constant	xtindex12:					std_logic_vector(11 downto 0)	:= x"7a7";
constant	xtindex13:					std_logic_vector(11 downto 0)	:= x"7a8";
constant	xtindex14:					std_logic_vector(11 downto 0)	:= x"7a9";

constant	test:						std_logic_vector(11 downto 0)	:= x"fff";
constant	waitstate:					std_logic_vector(11 downto 0)	:= x"ffe";

end;
--test bench written by Alban Bourge @ TIMA
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.pkg_tb.all;

entity prog is
	port(
				clock      : in std_logic;
				reset      : in std_logic;
				step       : in std_logic;
				instr_next : out instruction
			);
end prog;

architecture rtl of prog is

	signal instr_n : instruction := instr_rst;

	--Table describing fsm behavior
	constant fsm_behavior : table_behavior := (
		--##PROGRAM_GOES_DOWN_HERE##--
		0 => (state => Rst, context_uut => "00", arg => to_unsigned(0,ARG_WIDTH)),
		1 => (state => Rst, context_uut => "00", arg => to_unsigned(0,ARG_WIDTH)),
		2 => (state => Sig_start, context_uut => "01", arg => to_unsigned(0,ARG_WIDTH)),
		3 => (state => Ack_data, context_uut => "01", arg => to_unsigned(64,ARG_WIDTH)),
		4 => (state => Cp_search, context_uut => "01", arg => to_unsigned(0,ARG_WIDTH)),
		5 => (state => Idle, context_uut => "00", arg => to_unsigned(5,ARG_WIDTH)),
		6 => (state => Rst_uut, context_uut => "00", arg => to_unsigned(0,ARG_WIDTH)),
		7 => (state => Idle, context_uut => "00", arg => to_unsigned(5,ARG_WIDTH)),
		8 => (state => Sig_start, context_uut => "10", arg => to_unsigned(0,ARG_WIDTH)),
		9 => (state => Ack_data, context_uut => "10", arg => to_unsigned(64,ARG_WIDTH)),
		10 => (state => Running, context_uut => "10", arg => to_unsigned(20,ARG_WIDTH)),
		11 => (state => Cp_search, context_uut => "10", arg => to_unsigned(0,ARG_WIDTH)),
		12 => (state => Idle, context_uut => "00", arg => to_unsigned(5,ARG_WIDTH)),
		13 => (state => Rst_uut, context_uut => "00", arg => to_unsigned(0,ARG_WIDTH)),
		14 => (state => Idle, context_uut => "00", arg => to_unsigned(5,ARG_WIDTH)),
		15 => (state => Rest_ini0, context_uut => "01", arg => to_unsigned(0,ARG_WIDTH)),
		16 => (state => Waitfor, context_uut => "01", arg => to_unsigned(64,ARG_WIDTH)),
		17 => (state => Idle, context_uut => "00", arg => to_unsigned(5,ARG_WIDTH)),
		18 => (state => Rst_uut, context_uut => "00", arg => to_unsigned(0,ARG_WIDTH)),
		19 => (state => Idle, context_uut => "00", arg => to_unsigned(5,ARG_WIDTH)),
		20 => (state => Rest_ini0, context_uut => "10", arg => to_unsigned(0,ARG_WIDTH)),
		21 => (state => Waitfor, context_uut => "10", arg => to_unsigned(64,ARG_WIDTH)),
		22 => (state => Stop, context_uut => "00", arg => to_unsigned(0,ARG_WIDTH)),
		--##PROGRAM_GOES_OVER_HERE##--
		others => instr_rst);

	signal pc : unsigned(PC_SIZE - 1 downto 0) := (others => '0');

begin

	drive_state : process (reset,clock) is
	begin
		if reset = '1' then
			instr_n <= instr_rst;
			pc      <= (others => '0');
		elsif rising_edge(clock) then
			if (step = '1') then
				pc <= pc + 1;
			end if;
			instr_n <= fsm_behavior(to_integer(pc));
		end if;
	end process drive_state;

	--instr_next <= instr_n;
	instr_next <= fsm_behavior(to_integer(pc));

end rtl;

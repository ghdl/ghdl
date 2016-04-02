--test bench written by alban bourge @ tima
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pkg_tb is

	--fsm state types
	type state_t is (Rst,Sig_start,Ack_data,Running,Waitfor,Cp_search,Cp_save,Idle,Rst_uut,Rest_ini0,Rest_ini1,Rest,Stop);
	--context descriptor
	subtype context_t is std_logic_vector(1 downto 0);
	--argument width and type of fsm instruction
	constant ARG_WIDTH : integer := 8;
	subtype argument_t is unsigned(ARG_WIDTH - 1 downto 0);

	type instruction is 
		record
			state       : state_t;
			context_uut : context_t;
			arg         : argument_t;
		end record;

	--reset instruction
	constant instr_rst : instruction := (state => Rst, context_uut => (others =>'0'), arg => (others =>'0'));

	--ram instruction
	type ram_instruction is
		record
			sel     : std_logic;
			we      : std_logic;
			addr_up : std_logic;
			addr_z  : std_logic;
		end record;

	constant ram_instr_z : ram_instruction := (sel => '0', we => '0', addr_up => '0', addr_z => '0');

	--assert unit instruction
	type assert_instruction is
		record
			en_feed  : std_logic;
			en_check : std_logic;
		end record;

	constant assert_instr_z : assert_instruction := (en_feed => '0', en_check => '0');
	
	--size of instruction table defined by PC_SIZE i.e. width of program counter
	constant PC_SIZE : integer := 5;
	type table_behavior is array (0 to 2**PC_SIZE - 1) of instruction;

	--constraint fixed by unit under test (augh dependant)
	--##CONSTRAINTS_START##--
	subtype stdin_vector is std_logic_vector(31 downto 0);
	subtype stdout_vector is std_logic_vector(31 downto 0);
	subtype cp_vector is std_logic_vector(63 downto 0);
	--##CONSTRAINTS_END##--

	--assert_uut vector number counter size
	constant VEC_NO_SIZE : integer := 20;

end pkg_tb;

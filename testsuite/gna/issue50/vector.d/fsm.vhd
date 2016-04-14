--test bench written by Alban Bourge @ TIMA
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
library work;
use work.pkg_tb.all;

entity fsm is
	port(
				clock      : in std_logic;
				reset      : in std_logic;
				--prog interface
				instr_next : in instruction;
				step       : out std_logic;
				--uut interface
				cp_ok      : in std_logic;
				stdin_rdy  : in std_logic;
				stdin_ack  : in std_logic;
				reset_fsm  : out std_logic;
				start      : out std_logic;
				cp_en      : out std_logic;
				cp_rest    : out std_logic;
				--ram interface
				ram_1      : out ram_instruction;
				ram_2      : out ram_instruction;
				--assert_uut interface
				context_uut : out context_t;
				en_feed    : out std_logic;
				en_check   : out std_logic;
				vecs_found : in std_logic;
				vec_read   : in std_logic;
				--tb interface
				stopped    : out std_logic
			);
end fsm;

architecture rtl of fsm is

	-- read output
	signal step_sig  : std_logic;
	-- FSM signals
	signal instr_c   : instruction := instr_rst;
	signal instr_n   : instruction := instr_rst;
	-- TIMER signal
	signal times_en  : std_logic := '0';
	signal times_z   : std_logic := '0';
	signal times     : unsigned(ARG_WIDTH - 1 downto 0);
	signal times_max : unsigned(ARG_WIDTH - 1 downto 0);
	signal times_ok  : std_logic := '0';
	-- COUNTER signal
	signal count_en  : std_logic := '0';
	signal count_z   : std_logic := '0';
	signal count     : unsigned(ARG_WIDTH - 1 downto 0);
	signal count_max : unsigned(ARG_WIDTH - 1 downto 0);
	signal count_ok  : std_logic := '0';
	-- runtime counter
	signal runtime_en : std_logic := '0';
	signal runtime    : integer range 0 to 99999999; --100 million cycles

begin

	-- FSM
	state_reg : process (clock, reset) is
	begin
		if (reset = '1') then
			instr_c <= instr_rst;
		elsif rising_edge(clock) then
			instr_c <= instr_n;
		end if;
	end process state_reg;

	comb_logic: process(instr_next, instr_c, stdin_rdy, count_ok, times_ok, cp_ok, stdin_ack, vecs_found, vec_read)
	begin
		--default definition for fsm control signals
		instr_n    <= instr_rst;
		step_sig   <= '0';
		--top
		reset_fsm  <= '0';
		start      <= '0';
		cp_en      <= '0';
		cp_rest    <= '0';
		--counter & timer
		times_en   <= '0';
		times_max  <= (others => '0');
		count_en   <= '0';
		count_max  <= (others => '0');
		--runtime counter
		runtime_en <= '0';
		--ram
		ram_1      <= ram_instr_z;
		ram_2      <= ram_instr_z;
		--assert_uut
		en_feed    <= '0';
		en_check   <= '0';
		--tb interface
		stopped    <= '0';

		case instr_c.state is
			when Rst =>
				--signals
				reset_fsm    <= '1';
				ram_1.addr_z <= '1';
				ram_2.addr_z <= '1';
				step_sig     <= '1'; --demand for next instruction
				--transition
				instr_n <= instr_next;

			when Sig_start =>
				--signals
				start    <= '1';
				step_sig <= '1'; --demand for next instruction
				--transition
				instr_n <= instr_next;
				--if (instr_next.state = Ack_data) then
					--en_feed <= '1';
				--end if;

			when Ack_data =>
				times_max <= instr_c.arg - 1;
				--signals
				en_feed <= '1';
				--transition
				if (stdin_rdy = '1' and stdin_ack = '1') then
					times_en <= '1';
				end if;
				if (times_ok = '1') then
					en_feed <= '0';
					step_sig <= '1';
					instr_n <= instr_next;
				else
					instr_n <= instr_c;
				end if;

			when Running =>
				--signals
				count_max <= instr_c.arg;
				count_en  <= '1';
				--en_check  <= '1';
				--runtime counter
				if(vecs_found = '0') then
					runtime_en <= '1';
				end if;
				--transition
				if (count_ok = '1') then
					step_sig <= '1';
					instr_n  <= instr_next;
				else
					instr_n <= instr_c;
				end if;

			when Waitfor =>
				--signals
				count_max <= instr_c.arg;
				en_check  <= '1';
				if(vec_read = '1') then
					count_en  <= '1';
				end if;
				--runtime counter
				if(vecs_found = '0') then
					runtime_en <= '1';
				end if;
				--transition
				if (count_ok = '1') then
					step_sig <= '1';
					instr_n  <= instr_next;
				else
					instr_n <= instr_c;
				end if;

			when Cp_search =>
				--signals
				cp_en <= '1';
				--transition
				if (cp_ok = '1') then
					case instr_c.context_uut is
						when "01" =>
							ram_1.we <= '1';
							ram_1.addr_up  <= '1';
							ram_1.sel  <= '1';
						when "10" =>
							ram_2.we <= '1';
							ram_2.addr_up  <= '1';
							ram_2.sel  <= '1';
						when others =>
					end case;
					instr_n  <= (state => Cp_save, context_uut => instr_c.context_uut, arg => (others => '0')); --hard coded
				else
					instr_n <= instr_c;
				end if;

			when Cp_save =>
				--signals
				cp_en    <= '1';
				case instr_c.context_uut is
					when "01" =>
						ram_1.we <= '1';
						ram_1.addr_up  <= '1';
						ram_1.sel  <= '1';
					when "10" =>
						ram_2.we <= '1';
						ram_2.addr_up  <= '1';
						ram_2.sel  <= '1';
					when others =>
				end case;
				--transition
				if (cp_ok = '0') then
					case instr_c.context_uut is
						when "01" =>
							ram_1.we <= '0';
							ram_1.addr_up  <= '0';
						when "10" =>
							ram_2.we <= '0';
							ram_2.addr_up  <= '0';
						when others =>
					end case;
					step_sig <= '1';
					instr_n  <= instr_next;
				else
					instr_n  <= instr_c;
				end if;

			when Idle => 
				--signals
				count_max <= instr_c.arg;
				count_en <= '1';
				--transition
				if (count_ok = '1') then
					step_sig <= '1';
					instr_n <= instr_next;
				else
					instr_n <= instr_c;
				end if;

			when Rst_uut =>
				--signals
				reset_fsm <= '1';
				ram_1.addr_z <= '1';
				ram_2.addr_z <= '1';
				--transition
				step_sig <= '1';
				instr_n <= instr_next;

			when Rest_ini0 =>
				--signals
				start   <= '1';
				cp_en   <= '1';
				cp_rest <= '1';
				--this is for restoration : reading the first word of the right memory
				case instr_c.context_uut is
					when "01" =>
						ram_1.sel  <= '1';
					when "10" =>
						ram_2.sel  <= '1';
					when others =>
				end case;
				--transition
				instr_n <= (state => Rest_ini1, context_uut => instr_c.context_uut, arg => (others => '0')); --hard coded

			when Rest_ini1 =>
				--signals
				cp_en   <= '1';
				cp_rest <= '1';
				case instr_c.context_uut is
					when "01" =>
						ram_1.addr_up  <= '1';
						ram_1.sel  <= '1';
					when "10" =>
						ram_2.addr_up  <= '1';
						ram_2.sel  <= '1';
					when others =>
				end case;
				--transition
				instr_n <= (state => Rest, context_uut => instr_c.context_uut, arg => (others => '0')); --hard coded

			when Rest =>
				--signals
				cp_en   <= '1';
				cp_rest <= '1';
				case instr_c.context_uut is
					when "01" =>
						ram_1.addr_up  <= '1';
						ram_1.sel  <= '1';
					when "10" =>
						ram_2.addr_up  <= '1';
						ram_2.sel  <= '1';
					when others =>
				end case;
				--transition
				if (cp_ok = '0') then
					step_sig <= '1';
					instr_n  <= instr_next;
				else
					instr_n  <= instr_c;
				end if;

			when Stop =>
				--signals
				stopped   <= '1';
				reset_fsm <= '1';
				report "RUNTIME:" & integer'image(runtime);
				assert (vecs_found = '0')
				report "END_OF_SIM ---> Stop state reached, some output vectors were read." severity note;
				--transition
				instr_n <= (state => Stop, context_uut => "00", arg => (others => '0')); --hard coded

			when others =>
		end case;
	end process comb_logic;

	--*ER reset combo logic
	--if a step_sig signal is sent, it means a instr_next will be consumed
	reseter : process(step_sig)
	begin
		if (step_sig = '0') then
			times_z    <= '0';
			count_z    <= '0';
		else
			times_z    <= '1';
			count_z    <= '1';
		end if;
	end process reseter;

	--TIMER
	timer : process(clock, reset)
	begin
		if (reset =  '1') then
			times    <= (others => '0');
			times_ok <= '0';
		elsif rising_edge(clock) then
			if (times_z = '1') then
				times    <= (others => '0');
				times_ok <= '0';
			else
				if (times_en = '1') then
					times <= times + 1;
					if (times = times_max) then
						times_ok <= '1';
					else
						times_ok <= '0';
					end if;
				end if;
			end if;
		end if;
	end process timer;

	--COUNTER
	counter : process(clock, reset)
	begin
		if (reset =  '1') then
			count <= (others => '0');
			count_ok <= '0';
		elsif rising_edge(clock) then
			--count_ok driving if
			if (count_z = '1') then
				count_ok <= '0';
				count    <= (others => '0');
			else
				if (count = count_max) then
					count_ok <= '1';
				else
					count_ok <= '0';
					if (count_en = '1') then
						count <= count + 1;
					end if;
				end if;
			end if;
		end if;
		end process counter;

	--Runtime counter
	runtime_counter : process(clock, reset)
	begin
		if (reset =  '1') then
			runtime <= 0;
		elsif rising_edge(clock) then
			if (runtime_en = '1') then
				runtime <= runtime + 1;
				if ((runtime mod 1000) = 0) then
					report "Running since:" & integer'image(runtime) severity note;
				end if;
			end if;
		end if;
	end process runtime_counter;

	-- process only used for reporting current instruction
	reporter : process(instr_c)
	begin
		--report "Instruction: " & state_t'image(instr_c.state) severity note;
		report "Instruction: " & state_t'image(instr_c.state) & " (context " & integer'image(to_integer(unsigned(instr_c.context_uut))) & ")" severity note;
	end process reporter;

	--Combinational
	step <= step_sig;
	context_uut <= instr_c.context_uut;
end rtl;

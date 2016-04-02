-- written by Alban Bourge @ TIMA
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.pkg_tb.all;

entity cp3_test is
	port(
				clock   : in std_logic;
				reset   : in std_logic;
				n_error : out std_logic;
				stopped : out std_logic
			);
end cp3_test;

architecture rtl of cp3_test is

	--TOP signals
	signal reset_top   : std_logic := '0';
	signal stdin_data  : stdin_vector;
	signal stdin_rdy   : std_logic;
	signal stdin_ack   : std_logic;
	signal stdout_data : stdout_vector;
	signal stdout_rdy  : std_logic;
	signal stdout_ack  : std_logic;
	--ASSERT_UNIT signals
	signal context_uut : context_t;
	signal en_feed     : std_logic;
	signal en_check    : std_logic;
	signal n_error_s   : std_logic;
	signal vecs_found  : std_logic;
	signal vec_read    : std_logic;
	--PROG unit signals
	signal instr_next  : instruction;
	-- FSM unit signals
	signal step        : std_logic;
	signal start       : std_logic;
	-- FSM signals
	signal reset_fsm   : std_logic;
	signal stopped_s   : std_logic;


--------------------------------------
--   PART OF ARCHITECTURE WITH CP3  --

	--TOP signals
	signal cp_en       : std_logic := '0';
	signal cp_rest     : std_logic := '0';
	signal cp_ok       : std_logic;
	signal cp_din      : cp_vector := (others => '0');
	signal cp_dout     : cp_vector;
	--RAM signals
	signal ram_1       : ram_instruction;
	signal ram_2       : ram_instruction;
	signal address1    : std_logic_vector(12 downto 0) := (others => '0');
	signal address2    : std_logic_vector(12 downto 0) := (others => '0');
	signal datain      : cp_vector := (others => '0');
	signal dout1       : cp_vector;
	signal dout2       : cp_vector;

	--dut component declaration
	component top is
		port (
			clock : in  std_logic;
			reset : in  std_logic;
			start : in  std_logic;
			stdin_data : in  stdin_vector;
			stdin_rdy : out std_logic;
			stdin_ack : in  std_logic;
			stdout_data : out stdout_vector;
			stdout_rdy : out std_logic;
			stdout_ack : in  std_logic;
			cp_en : in  std_logic;
			cp_rest : in  std_logic;
			cp_din : in  cp_vector;
			cp_dout : out cp_vector;
			cp_ok : out std_logic
		);
	end component top;

begin

	uut : entity work.top(augh)
	port map(
						clock       => clock,
						reset       => reset_top,
						start       => start,
						stdin_data  => stdin_data,
						stdin_rdy   => stdin_rdy,
						stdin_ack   => stdin_ack,
						cp_en       => cp_en,
						cp_rest     => cp_rest,
						cp_ok       => cp_ok,
						cp_din      => cp_din,
						cp_dout     => cp_dout,
						stdout_data => stdout_data,
						stdout_rdy  => stdout_rdy,
						stdout_ack  => stdout_ack
					);

	ram1 : entity work.sync_ram(rtl)
	port map(
						clock   => clock,
						we      => ram_1.we,
						address => address1,
						datain  => datain,
						dataout => dout1
					);

	ram2 : entity work.sync_ram(rtl)
	port map(
						clock   => clock,
						we      => ram_2.we,
						address => address2,
						datain  => datain,
						dataout => dout2
					);

	fsm_unit : entity work.fsm(rtl)
	port map(
						clock      => clock,
						reset      => reset,
						--prog interface
						instr_next => instr_next,
						step       => step,
						--uut interface
						cp_ok      => cp_ok,
						stdin_rdy  => stdin_rdy,
						stdin_ack  => stdin_ack,
						reset_fsm  => reset_fsm,
						start      => start,
						cp_en      => cp_en,
						cp_rest    => cp_rest,
						--ram interface
						ram_1      => ram_1,
						ram_2      => ram_2,
						--assert_uut interface
						context_uut => context_uut,
						en_feed    => en_feed,
						en_check   => en_check,
						vecs_found => vecs_found,
						vec_read   => vec_read,
						--tb interface
						stopped    => stopped_s
					);

	--RAM ADDRESS controller 1
	ram_ctrl1 : process(clock, reset)
	begin
		if (reset = '1') then
			address1 <= (others => '0');
		elsif rising_edge(clock) then
			if (ram_1.addr_z = '1') then
				address1 <= (others => '0');
			elsif (ram_1.addr_up = '1') then
				address1 <= std_logic_vector(unsigned(address1) + 1);
			end if;
		end if;
	end process ram_ctrl1;

	--RAM ADDRESS controller 2
	ram_ctrl2 : process(clock, reset)
	begin
		if (reset = '1') then
			address2 <= (others => '0');
		elsif rising_edge(clock) then
			if (ram_2.addr_z = '1') then
				address2 <= (others => '0');
			elsif (ram_2.addr_up = '1') then
				address2 <= std_logic_vector(unsigned(address2) + 1);
			end if;
		end if;
	end process ram_ctrl2;

	--other comb signals
	datain <= cp_dout;
	cp_din <= dout2 when ram_2.sel = '1' else dout1;

--   END OF ARCHITECTURE WITH CP3  --
--------------------------------------

--------------------------------------
-- PART OF ARCHITECTURE WITHOUT CP3 --
--
--	--dut component declaration
--	component top is
--		port (
--			clock : in  std_logic;
--			reset : in  std_logic;
--			start : in  std_logic;
--			stdin_data : in  stdin_vector;
--			stdin_rdy : out std_logic;
--			stdin_ack : in  std_logic;
--			stdout_data : out stdout_vector;
--			stdout_rdy : out std_logic;
--			stdout_ack : in  std_logic
--		);
--	end component top;
--
--begin
--
--	uut : entity work.top(augh)
--	port map(
--						clock       => clock,
--						reset       => reset_top,
--						start       => start,
--						stdin_data  => stdin_data,
--						stdin_rdy   => stdin_rdy,
--						stdin_ack   => stdin_ack,
--						stdout_data => stdout_data,
--						stdout_rdy  => stdout_rdy,
--						stdout_ack  => stdout_ack
--					);
--
--	fsm_unit : entity work.fsm(rtl)
--	port map(
--						clock      => clock,
--						reset      => reset,
--						--prog interface
--						instr_next => instr_next,
--						step       => step,
--						--uut interface
--						cp_ok      => '0',
--						stdin_rdy  => stdin_rdy,
--						stdin_ack  => stdin_ack,
--						reset_fsm  => reset_fsm,
--						start      => start,
--						cp_en      => open,
--						cp_rest    => open,
--						--ram interface
--						ram_1      => open,
--						ram_2      => open,
--						--assert_uut interface
--						context_uut => context_uut,
--						en_feed    => en_feed,
--						en_check   => en_check,
--						vecs_found => vecs_found,
--						vec_read   => vec_read,
--						--tb interface
--						stopped    => stopped_s
--					);
--
-- END OF ARCHITECTURE WITHOUT CP3 --
--------------------------------------

	assert_unit : entity work.assert_uut(rtl)
	port map(
						clock       => clock,
						reset       => reset,
						context_uut => context_uut,
						en_feed     => en_feed,
						stdin_rdy   => stdin_rdy,
						stdin_ack   => stdin_ack,
						stdin_data  => stdin_data,
						en_check    => en_check,
						stdout_rdy  => stdout_rdy,
						stdout_ack  => stdout_ack,
						stdout_data => stdout_data,
						vecs_found  => vecs_found,
						vec_read    => vec_read,
						n_error     => n_error_s
					);

	prog_unit : entity work.prog(rtl)
	port map(
						clock      => clock,
						reset      => reset,
						step       => step,
						instr_next => instr_next
					);

	--other comb signals
	reset_top <= reset or reset_fsm;

	--outputs
	n_error <= n_error_s;
	stopped <= stopped_s;

end rtl;

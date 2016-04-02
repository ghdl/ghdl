--test bench written by Alban Bourge @ TIMA
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
library work;
use work.pkg_tb.all;

entity assert_uut is
	port(
				clock        : in std_logic;
				reset        : in std_logic;
				context_uut  : in context_t;
				en_feed      : in std_logic;
				stdin_rdy    : in std_logic;
				stdin_ack    : out std_logic;
				stdin_data   : out stdin_vector;
				en_check     : in std_logic;
				stdout_rdy   : in std_logic;
				stdout_ack   : out std_logic;
				stdout_data  : in stdout_vector;
				vecs_found   : out std_logic;
				vec_read     : out std_logic;
				n_error      : out std_logic
			);
end assert_uut;

architecture rtl of assert_uut is

	type vin_table  is array(0 to 2**VEC_NO_SIZE - 1) of stdin_vector;
	type vout_table is array(0 to 2**VEC_NO_SIZE - 1) of stdout_vector;
	constant input_vectors_1 : vin_table := (
		--##INPUT_VECTORS_1_GO_DOWN_HERE##--
		0 => x"00_00_00_07",
		1 => x"00_00_00_03",
		--##INPUT_VECTORS_1_GO_OVER_HERE##--
		others => (others => '0'));
	constant output_vectors_1 : vout_table := (
		--##OUTPUT_VECTORS_1_GO_DOWN_HERE##--
		0 => x"00_00_00_16",
		--##OUTPUT_VECTORS_1_GO_OVER_HERE##--
		others => (others => '0'));
	constant input_vectors_2 : vin_table := (
		--##INPUT_VECTORS_2_GO_DOWN_HERE##--
		0 => x"00_00_00_07",
		1 => x"00_00_00_03",
		--##INPUT_VECTORS_2_GO_OVER_HERE##--
		others => (others => '0'));
	constant output_vectors_2 : vout_table := (
		--##OUTPUT_VECTORS_2_GO_DOWN_HERE##--
		0 => x"00_00_00_16",
		--##OUTPUT_VECTORS_2_GO_OVER_HERE##--
		others => (others => '0'));

	signal out_vec_counter_1 : unsigned(VEC_NO_SIZE - 1 downto 0);
	signal out_vec_counter_2 : unsigned(VEC_NO_SIZE - 1 downto 0);
	signal stdin_ack_sig  : std_logic;
	signal vector_read    : std_logic;

begin

	feed : process(reset, clock) is
	variable in_vec_counter_1  : unsigned(VEC_NO_SIZE - 1 downto 0);
	variable in_vec_counter_2  : unsigned(VEC_NO_SIZE - 1 downto 0);
	begin
		if (reset = '1') then
			in_vec_counter_1 := (others => '0');
			in_vec_counter_2 := (others => '0');
			stdin_data       <= (others => '0');
			stdin_ack_sig    <= '0';
		elsif rising_edge(clock) then
			case context_uut is
				when "01" =>
					if (en_feed = '1') then
						stdin_data <= input_vectors_1(to_integer(in_vec_counter_1));
						stdin_ack_sig  <= '1';
						if (stdin_rdy = '1' and stdin_ack_sig = '1') then
							in_vec_counter_1 := in_vec_counter_1 + 1;
							stdin_ack_sig  <= '0';
						end if;
					else
						--in_vec_counter_1 <= (others => '0');
						stdin_data     <= (others => '0');
						stdin_ack_sig  <= '0';
					end if;
				when "10" =>
					if (en_feed = '1') then
						stdin_data <= input_vectors_2(to_integer(in_vec_counter_2));
						stdin_ack_sig  <= '1';
						if (stdin_rdy = '1' and stdin_ack_sig = '1') then
							in_vec_counter_2 := in_vec_counter_2 + 1;
							stdin_ack_sig  <= '0';
						end if;
					else
						--in_vec_counter_2 <= (others => '0');
						stdin_data     <= (others => '0');
						stdin_ack_sig  <= '0';
					end if;
				when others =>
			end case;
		end if;
	end process feed;

	check : process(reset, clock) is
	begin
		if (reset = '1') then
			n_error     <= '1';
			vec_read <= '0';
		elsif rising_edge(clock) then
			vec_read <= '0';
			if (en_check = '1') then
				if (stdout_rdy = '1') then
					vec_read <= '1';
					case context_uut is
						when "01" =>
							assert (stdout_data = output_vectors_1(to_integer(out_vec_counter_1)))
							report "ERROR ---> Bad output vector found";
							--synthesizable check
							if (stdout_data /= output_vectors_1(to_integer(out_vec_counter_1))) then
								n_error <= '0';
							end if;
						when "10" =>
							assert (stdout_data = output_vectors_2(to_integer(out_vec_counter_2)))
							report "ERROR ---> Bad output vector found";
							--synthesizable check
							if (stdout_data /= output_vectors_2(to_integer(out_vec_counter_2))) then
								n_error <= '0';
							end if;
						when others =>
					end case;
				end if;
			end if;
		end if;
	end process check;

	read_counter : process(reset, clock) is
	begin
		if (reset = '1') then
			out_vec_counter_1 <= (others => '0');
			out_vec_counter_2 <= (others => '0');
		elsif rising_edge(clock) then
			if (en_check = '1') then
				if (stdout_rdy = '1') then
					case context_uut is
						when "01" =>
							out_vec_counter_1 <= out_vec_counter_1 + 1;
						when "10" =>
							out_vec_counter_2 <= out_vec_counter_2 + 1;
						when others =>
					end case;
				end if;
			--else
			--	case context_uut is
			--		when "01" =>
			--			out_vec_counter_1 <= (others => '0');
			--		when "10" =>
			--			out_vec_counter_2 <= (others => '0');
			--		when others =>
			--	end case;
			end if;
		end if;
	end process read_counter;

	--asynchronous declarations
	stdout_ack <= en_check;
	stdin_ack <= stdin_ack_sig;
	vecs_found <= '1' when (out_vec_counter_1 /= 0 or out_vec_counter_2 /= 0) else '0';

end rtl;

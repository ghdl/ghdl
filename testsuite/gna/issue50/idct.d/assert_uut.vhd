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
		0 => x"00_00_00_a3",
		1 => x"00_00_00_ea",
		2 => x"00_00_00_cc",
		3 => x"00_00_00_28",
		4 => x"00_00_00_30",
		5 => x"00_00_00_a0",
		6 => x"00_00_00_c0",
		7 => x"00_00_00_80",
		8 => x"00_00_00_00",
		9 => x"00_00_00_00",
		10 => x"00_00_00_00",
		11 => x"00_00_00_00",
		12 => x"00_00_00_00",
		13 => x"00_00_00_00",
		14 => x"00_00_00_00",
		15 => x"00_00_00_00",
		16 => x"00_00_00_00",
		17 => x"00_00_00_00",
		18 => x"00_00_00_00",
		19 => x"00_00_00_00",
		20 => x"00_00_00_00",
		21 => x"00_00_00_00",
		22 => x"00_00_00_00",
		23 => x"00_00_00_00",
		24 => x"00_00_00_00",
		25 => x"00_00_00_00",
		26 => x"00_00_00_00",
		27 => x"00_00_00_00",
		28 => x"00_00_00_00",
		29 => x"00_00_00_00",
		30 => x"00_00_00_00",
		31 => x"00_00_00_00",
		32 => x"00_00_00_00",
		33 => x"00_00_00_00",
		34 => x"00_00_00_00",
		35 => x"00_00_00_00",
		36 => x"00_00_00_00",
		37 => x"00_00_00_00",
		38 => x"00_00_00_00",
		39 => x"00_00_00_00",
		40 => x"00_00_00_00",
		41 => x"00_00_00_00",
		42 => x"00_00_00_00",
		43 => x"00_00_00_00",
		44 => x"00_00_00_00",
		45 => x"00_00_00_00",
		46 => x"00_00_00_00",
		47 => x"00_00_00_00",
		48 => x"00_00_00_00",
		49 => x"00_00_00_00",
		50 => x"00_00_00_00",
		51 => x"00_00_00_00",
		52 => x"00_00_00_00",
		53 => x"00_00_00_00",
		54 => x"00_00_00_00",
		55 => x"00_00_00_00",
		56 => x"00_00_00_00",
		57 => x"00_00_00_00",
		58 => x"00_00_00_00",
		59 => x"00_00_00_00",
		60 => x"00_00_00_00",
		61 => x"00_00_00_00",
		62 => x"00_00_00_00",
		63 => x"00_00_00_00",
		--##INPUT_VECTORS_1_GO_OVER_HERE##--
		others => (others => '0'));
	constant output_vectors_1 : vout_table := (
		--##OUTPUT_VECTORS_1_GO_DOWN_HERE##--
		0 => x"ff",
		1 => x"00",
		2 => x"ff",
		3 => x"ff",
		4 => x"00",
		5 => x"00",
		6 => x"ff",
		7 => x"00",
		8 => x"ff",
		9 => x"00",
		10 => x"ff",
		11 => x"ff",
		12 => x"00",
		13 => x"00",
		14 => x"ff",
		15 => x"00",
		16 => x"ff",
		17 => x"00",
		18 => x"ff",
		19 => x"ff",
		20 => x"00",
		21 => x"00",
		22 => x"ff",
		23 => x"00",
		24 => x"ff",
		25 => x"00",
		26 => x"ff",
		27 => x"ff",
		28 => x"00",
		29 => x"00",
		30 => x"ff",
		31 => x"00",
		32 => x"ff",
		33 => x"00",
		34 => x"ff",
		35 => x"ff",
		36 => x"00",
		37 => x"00",
		38 => x"ff",
		39 => x"00",
		40 => x"ff",
		41 => x"00",
		42 => x"ff",
		43 => x"ff",
		44 => x"00",
		45 => x"00",
		46 => x"ff",
		47 => x"00",
		48 => x"ff",
		49 => x"00",
		50 => x"ff",
		51 => x"ff",
		52 => x"00",
		53 => x"00",
		54 => x"ff",
		55 => x"00",
		56 => x"ff",
		57 => x"00",
		58 => x"ff",
		59 => x"ff",
		60 => x"00",
		61 => x"00",
		62 => x"ff",
		63 => x"00",
		--##OUTPUT_VECTORS_1_GO_OVER_HERE##--
		others => (others => '0'));
	constant input_vectors_2 : vin_table := (
		--##INPUT_VECTORS_2_GO_DOWN_HERE##--
		0 => x"00_00_00_a3",
		1 => x"00_00_00_ea",
		2 => x"00_00_00_cc",
		3 => x"00_00_00_28",
		4 => x"00_00_00_30",
		5 => x"00_00_00_a0",
		6 => x"00_00_00_c0",
		7 => x"00_00_00_80",
		8 => x"00_00_00_00",
		9 => x"00_00_00_00",
		10 => x"00_00_00_00",
		11 => x"00_00_00_00",
		12 => x"00_00_00_00",
		13 => x"00_00_00_00",
		14 => x"00_00_00_00",
		15 => x"00_00_00_00",
		16 => x"00_00_00_00",
		17 => x"00_00_00_00",
		18 => x"00_00_00_00",
		19 => x"00_00_00_00",
		20 => x"00_00_00_00",
		21 => x"00_00_00_00",
		22 => x"00_00_00_00",
		23 => x"00_00_00_00",
		24 => x"00_00_00_00",
		25 => x"00_00_00_00",
		26 => x"00_00_00_00",
		27 => x"00_00_00_00",
		28 => x"00_00_00_00",
		29 => x"00_00_00_00",
		30 => x"00_00_00_00",
		31 => x"00_00_00_00",
		32 => x"00_00_00_00",
		33 => x"00_00_00_00",
		34 => x"00_00_00_00",
		35 => x"00_00_00_00",
		36 => x"00_00_00_00",
		37 => x"00_00_00_00",
		38 => x"00_00_00_00",
		39 => x"00_00_00_00",
		40 => x"00_00_00_00",
		41 => x"00_00_00_00",
		42 => x"00_00_00_00",
		43 => x"00_00_00_00",
		44 => x"00_00_00_00",
		45 => x"00_00_00_00",
		46 => x"00_00_00_00",
		47 => x"00_00_00_00",
		48 => x"00_00_00_00",
		49 => x"00_00_00_00",
		50 => x"00_00_00_00",
		51 => x"00_00_00_00",
		52 => x"00_00_00_00",
		53 => x"00_00_00_00",
		54 => x"00_00_00_00",
		55 => x"00_00_00_00",
		56 => x"00_00_00_00",
		57 => x"00_00_00_00",
		58 => x"00_00_00_00",
		59 => x"00_00_00_00",
		60 => x"00_00_00_00",
		61 => x"00_00_00_00",
		62 => x"00_00_00_00",
		63 => x"00_00_00_00",
		--##INPUT_VECTORS_2_GO_OVER_HERE##--
		others => (others => '0'));
	constant output_vectors_2 : vout_table := (
		--##OUTPUT_VECTORS_2_GO_DOWN_HERE##--
		0 => x"ff",
		1 => x"00",
		2 => x"ff",
		3 => x"ff",
		4 => x"00",
		5 => x"00",
		6 => x"ff",
		7 => x"00",
		8 => x"ff",
		9 => x"00",
		10 => x"ff",
		11 => x"ff",
		12 => x"00",
		13 => x"00",
		14 => x"ff",
		15 => x"00",
		16 => x"ff",
		17 => x"00",
		18 => x"ff",
		19 => x"ff",
		20 => x"00",
		21 => x"00",
		22 => x"ff",
		23 => x"00",
		24 => x"ff",
		25 => x"00",
		26 => x"ff",
		27 => x"ff",
		28 => x"00",
		29 => x"00",
		30 => x"ff",
		31 => x"00",
		32 => x"ff",
		33 => x"00",
		34 => x"ff",
		35 => x"ff",
		36 => x"00",
		37 => x"00",
		38 => x"ff",
		39 => x"00",
		40 => x"ff",
		41 => x"00",
		42 => x"ff",
		43 => x"ff",
		44 => x"00",
		45 => x"00",
		46 => x"ff",
		47 => x"00",
		48 => x"ff",
		49 => x"00",
		50 => x"ff",
		51 => x"ff",
		52 => x"00",
		53 => x"00",
		54 => x"ff",
		55 => x"00",
		56 => x"ff",
		57 => x"00",
		58 => x"ff",
		59 => x"ff",
		60 => x"00",
		61 => x"00",
		62 => x"ff",
		63 => x"00",
		--##OUTPUT_VECTORS_2_GO_OVER_HERE##--
		others => (others => '0'));
	signal in_vec_counter_1  : unsigned(VEC_NO_SIZE - 1 downto 0);
	signal in_vec_counter_2  : unsigned(VEC_NO_SIZE - 1 downto 0);
	signal out_vec_counter_1 : unsigned(VEC_NO_SIZE - 1 downto 0);
	signal out_vec_counter_2 : unsigned(VEC_NO_SIZE - 1 downto 0);

	signal stdin_ack_sig  : std_logic;
	signal vector_read    : std_logic;

begin

	feed : process(reset, clock) is
	begin
		if (reset = '1') then
			in_vec_counter_1 <= (others => '0');
			in_vec_counter_2 <= (others => '0');
			stdin_data       <= (others => '0');
			stdin_ack_sig    <= '0';
		elsif rising_edge(clock) then
			case context_uut is
				when "01" =>
					if (en_feed = '1') then
						stdin_data <= input_vectors_1(to_integer(in_vec_counter_1));
						stdin_ack_sig  <= '1';
						if (stdin_rdy = '1' and stdin_ack_sig = '1') then
							in_vec_counter_1 <= in_vec_counter_1 + 1;
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
							in_vec_counter_2 <= in_vec_counter_2 + 1;
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

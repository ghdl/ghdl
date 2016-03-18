library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.numeric_std.all;

entity sub_206 is
	port (
		gt : out std_logic;
		output : out std_logic_vector(40 downto 0);
		sign : in  std_logic;
		in_b : in  std_logic_vector(40 downto 0);
		in_a : in  std_logic_vector(40 downto 0)
	);
end sub_206;

architecture augh of sub_206 is

	signal carry_inA : std_logic_vector(42 downto 0);
	signal carry_inB : std_logic_vector(42 downto 0);
	signal carry_res : std_logic_vector(42 downto 0);

	-- Signals to generate the comparison outputs
	signal msb_abr  : std_logic_vector(2 downto 0);
	signal tmp_sign : std_logic;
	signal tmp_eq   : std_logic;
	signal tmp_le   : std_logic;
	signal tmp_ge   : std_logic;

begin

	-- To handle the CI input, the operation is '0' - CI
	-- If CI is not present, the operation is '0' - '0'
	carry_inA <= '0' & in_a & '0';
	carry_inB <= '0' & in_b & '0';
	-- Compute the result
	carry_res <= std_logic_vector(unsigned(carry_inA) - unsigned(carry_inB));

	-- Set the outputs
	output <= carry_res(41 downto 1);

	-- Other comparison outputs

	-- Temporary signals
	msb_abr <= in_a(40) & in_b(40) & carry_res(41);
	tmp_sign <= sign;
	tmp_eq  <= '1' when in_a = in_b else '0';

	tmp_le <=
		tmp_eq when msb_abr = "000" or msb_abr = "110" else
		'1' when msb_abr = "001" or msb_abr = "111" else
		'1' when tmp_sign = '0' and (msb_abr = "010" or msb_abr = "011") else
		'1' when tmp_sign = '1' and (msb_abr = "100" or msb_abr = "101") else
		'0';

	tmp_ge <=
		'1' when msb_abr = "000" or msb_abr = "110" else
		'1' when tmp_sign = '0' and (msb_abr = "100" or msb_abr = "101") else
		'1' when tmp_sign = '1' and (msb_abr = "010" or msb_abr = "011") else
		'0';

	gt <= not(tmp_le);

end architecture;

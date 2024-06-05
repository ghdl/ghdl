library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bin_to_7seg is
	port(
		clk_in: in std_logic;
		
		bin_in: in unsigned(3 downto 0);
		
		segs_1_out: out std_logic_vector(6 downto 0);
		segs_2_out: out std_logic_vector(6 downto 0)
	);
end;

architecture rtl of bin_to_7seg is
	type seg_a is array(0 to 15) of std_logic_vector(6 downto 0);
	constant seg_c: seg_a := (
		"1111110", -- 0
		"0110000", -- 1
		"1101101", -- 2
		"1111001", -- 3
		"0110011", -- 4
		"1011011", -- 5
		"1011111", -- 6
		"1110000", -- 7
		"1111111", -- 8
		"1111011", -- 9
		"1110111", -- A
		"0011111", -- B
		"1001110", -- C
		"0111101", -- D
		"1001111", -- E
		"1000111" -- F
	);
	
	signal cnt: integer range 0 to 6 := 6;
begin
	process(clk_in)
		variable s: std_logic_vector(6 downto 0);
	begin
		if rising_edge(clk_in) then
			if cnt < 6 then
				cnt <= cnt + 1;
			end if;
			if cnt = 6 then
				cnt <= 0;
			end if;

			segs_1_out <= (others => '0');
			s := seg_c(to_integer(bin_in));
			segs_1_out(cnt) <= s(cnt);
			
			segs_2_out <= (others => '0');
			segs_2_out(cnt) <= seg_c(to_integer(bin_in))(cnt);
		end if;
	end process;
	
	default clock is rising_edge(clk_in);
	
	a_1: assume always stable(bin_in);
	
	f_1: assert always {true; onehot0(segs_1_out)};
	
	c_1: cover {bin_in = 1; [*15]};
end;

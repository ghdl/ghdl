library ieee;
use ieee.std_logic_1164.all;

use work.int_arr.all;
use work.slv_arr.all;

entity window_splitter_1D is
	generic (
		-- INPUT_SIZE : integer;
		WINDOW_COUNT : integer;
		WINDOW_SIZE : integer;
		BIT_WIDTH : integer;
		WINDOW_STRIDE : integer
	);
	port (
		i_in 	: in slv_array_t;--(0 to INPUT_SIZE-1)(BIT_WIDTH-1 downto 0);
		o_windows : out slv_array_t(0 to WINDOW_COUNT*WINDOW_SIZE-1)(BIT_WIDTH-1 downto 0)
	);

	constant IN_LEN1 : integer := i_in'length;
	constant WINDOW_COUNT_1 : integer := 1 + (IN_LEN1 - WINDOW_SIZE)/WINDOW_STRIDE;
end entity;

architecture comb of window_splitter_1D is
begin
	gen_w1 : for c1 in 0 to WINDOW_COUNT_1-1 generate
		gen_wj : for j in 0 to WINDOW_SIZE-1 generate
			o_windows(c1*WINDOW_SIZE + j) <= i_in(c1*WINDOW_STRIDE+j);
		end generate gen_wj;
	end generate gen_w1;
end architecture;
-----------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.int_arr.all;
use work.slv_arr.all;

entity window_splitter_1D_tb is
	generic (
		WINDOW_SIZE 	: integer := 3;
		WINDOW_STRIDE : integer := 1;
		IN_SIZE 	: integer := 6; 
		BIT_WIDTH : positive := 16
	);

	constant WINDOW_COUNT : integer := 1 + (IN_SIZE - WINDOW_SIZE)/WINDOW_STRIDE;

	--function rand return integer is
	--begin assert false report "VHPIDIRECT rand" severity failure; end;
	--attribute foreign of rand : function is "VHPIDIRECT rand";

end entity;

architecture behav of window_splitter_1D_tb is
	signal s_in : slv_array_t(0 to IN_SIZE-1)(BIT_WIDTH-1 downto 0) := (others => (others => '1'));
	signal s_windows : slv_array_t(0 to WINDOW_COUNT*WINDOW_SIZE-1)(BIT_WIDTH-1 downto 0) := (others => (others => '0'));
begin
	s : entity work.window_splitter_1D
		generic map (	
			-- INPUT_SIZE => IN_SIZE,
			WINDOW_COUNT => WINDOW_COUNT,
			BIT_WIDTH => BIT_WIDTH,
			WINDOW_SIZE => WINDOW_SIZE,
			WINDOW_STRIDE => WINDOW_STRIDE
		)
		port map (
			i_in => s_in,
			o_windows => s_windows
		);

	process
		variable v_in : int_array_t(0 to IN_SIZE-1) := (others => 0);
		variable v_windows : int_array_t(0 to WINDOW_COUNT*WINDOW_SIZE-1) := (others => 0);
		variable v_in_val : integer := 0;
	begin

		for t in 1 to 100 loop

			for i in 0 to IN_SIZE-1 loop
				v_in(i) := (i * 17 + t * 5) mod 256 - 128;
				s_in(i) <= std_logic_vector(to_signed(v_in(i), BIT_WIDTH));
			end loop;
			
			wait for 10 ns;

			for i in 0 to IN_SIZE-1 loop
				v_in_val := to_integer(signed(s_in(i)));
				assert v_in_val = v_in(i) report "[" & integer'image(i) & "] = " & integer'image(v_in_val) & " != " & integer'image(v_in(i));
			end loop;

			for c1 in 0 to WINDOW_COUNT-1 loop
				for j in 0 to WINDOW_SIZE-1 loop
					v_in_val := to_integer(signed(s_windows(c1*WINDOW_SIZE + j)));
					assert s_windows(c1*WINDOW_SIZE + j) = s_in(c1*WINDOW_STRIDE+j) report "Test " & integer'image(t) & " 'window_splitter_1D': incorrect SLV on output."; 
					-- assert v_in_val = v_in(c1*WINDOW_STRIDE+j) 
					-- 	report "Test " & integer'image(t) & " 'window_splitter_1D': Window(" & integer'image(c1) & ")(" & integer'image(j) & ") = " 
					-- 									& integer'image(v_in_val) & " != " & integer'image(v_in(c1*WINDOW_STRIDE)) & " In(" & integer'image(c1*WINDOW_STRIDE+j) & ")." severity note;
				end loop;
			end loop;

		end loop;

		report "Test 'window_splitter_1D': Success.";
		wait;
	end process;

end architecture;

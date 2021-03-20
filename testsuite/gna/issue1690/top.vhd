
Library ieee;
use ieee.std_logic_1164.all;

entity tb_top is
end entity;


architecture tb of tb_top is

    signal a,b,c : std_logic;
    signal clk_sys : std_logic;

begin
	
    gen_clock_proc : process
    begin
        clk_sys <= '1';
        wait for 5 ns;
        clk_sys <= '0';
        wait for 5 ns;
    end process;
	
	test_proc : process
	begin
	    a <= '0';
	    b <= '0';
	    c <= '0';
	    wait until rising_edge(clk_sys);
	    a <= '1';
	    wait until rising_edge(clk_sys);
	    a <= '0';
	    b <= '1';
	    wait until rising_edge(clk_sys);
	    b <= '0';
	    c <= '1';
	    wait until rising_edge(clk_sys);
	    
        std.env.finish;
	end process;


    --  psl default clock is rising_edge(clk_sys);

    -- psl cov_simult_a_b_c : cover {a = '1' and b = '1' and c = '1'};

end architecture tb;


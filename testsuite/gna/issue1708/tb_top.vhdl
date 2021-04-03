Library ieee;
use ieee.std_logic_1164.all;

entity tb_top is
end entity;


architecture tb of tb_top is

    signal a,b : std_logic := '0';
    signal clk_sys : std_logic;

    default clock is rising_edge(clk_sys);
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
          a <= '1';
          wait for 50 ns;
          std.env.finish;
    end process;


    my_seq : assert never {a = '1'; b = '1'}[=3];

end architecture tb;

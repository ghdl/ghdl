library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use ieee.std_logic_textio.all;
use std.textio.all;

library work;
use work.test_pkg.all;


entity test_bench is

end entity;


architecture bench of test_bench is
    signal counter    : natural range 0 to 1000;
    signal my_signal  : sfixed(0 downto -15);
    signal my_array   : t_sf_array(0 to 15)(0 downto -15) := do_something(16, my_signal);
    signal s_rst      : std_logic := '1'; 
    signal s_clk      : std_logic := '1';

begin
    s_rst <= '0' after 50 ns;
    s_clk <= not s_clk after 10 ns;

    write_result : process (s_rst, s_clk) is
        file test_file   : text open write_mode is "output.txt";
        variable wrline  : line;
        variable linenum : integer := 0;
    begin
        if (s_rst = '1') then
            linenum := 0;
        elsif (rising_edge(s_clk) and counter < 16) then
            write(wrline, real'image(to_real(my_array(counter))));
            writeline(test_file, wrline);
        end if;
    end process;
end architecture;

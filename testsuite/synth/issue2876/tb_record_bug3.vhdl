library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;

entity tb_record_bug3 is
end tb_record_bug3;

architecture arch of tb_record_bug3 is

signal clk  : std_logic  := '0';
signal rst  : std_logic  := '1';
signal x, a : std_logic_vector(7 downto 0);
signal i, j : std_logic_vector(7 downto 0);

constant PERIOD : time := 10 ns;

begin

uut : entity work.record_bug3
    port map (
        clk_i   => clk,
        rst_i   => rst,
        x_o     => x,
        a_o     => a,
        i_o     => i,
        j_o     => j
    );

clk <= not clk after PERIOD/2;
rst <= '0' after PERIOD*4;

process
begin
    wait for PERIOD*100;
    for k in 0 to 100 loop
        assert x = i 
        report "Wrong 'x' signal value " & to_hstring(x) & " for " & to_hstring(i)
        severity failure;
        
        assert to_integer(unsigned(a)) = (to_integer(unsigned(i))+1)*(to_integer(unsigned(j))+1) 
        report "Wrong 'a' signal value " & to_hstring(a) & " for " & to_hstring(i) & "," & to_hstring(j) 
        severity failure;
        
        wait for PERIOD;
    end loop;
    report "Test OK!";
    finish;
end process;

end arch;


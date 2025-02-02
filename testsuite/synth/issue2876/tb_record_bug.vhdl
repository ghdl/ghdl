library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;

entity tb_record_bug is
end;

architecture arch of tb_record_bug is

signal clk  : std_logic  := '0';
signal rst  : std_logic  := '1';
signal test : std_logic_vector(7 downto 0);

begin

uut : entity work.record_bug
    port map (
        clk_i   => clk,
        rst_i   => rst,
        test_o  => test
    );

clk <= not clk after 5 ns;
rst <= '0' after 20 ns;

process
begin
    wait for 100 ns;
    assert test = X"AA" report "Wrong test signal value " & to_hstring(test) severity failure;
    report "Test OK!";
    finish;
end process;

end arch;

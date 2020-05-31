library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

entity tb_ent is
end tb_ent;

architecture behave of tb_ent is
    signal insn: std_ulogic_vector(31 downto 0);
    signal ispr1: std_ulogic_vector(5 downto 0);
    signal ispr2: std_ulogic_vector(5 downto 0);
begin
    test: entity work.ent
	port map (
	    insn_i => insn,
	    ispr1_o => ispr1,
	    ispr2_o => ispr2
	);

    test_process: process
    begin
	insn <= x"7d8903a6";
	wait for 1 ns;
	report " ispr1=" & to_hstring(ispr1);
	report " ispr2=" & to_hstring(ispr2);
        assert ispr1 = 6x"21" severity failure;
        assert ispr2 = 6x"2c" severity failure;
        report "end of test";
        wait;
    end process;
end behave;

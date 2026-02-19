library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_test is
end entity;

architecture behav of tb_test is
    signal s_clk    : std_ulogic := '0';
    signal s_in1    : std_ulogic := '0';
    signal s_in2    : std_ulogic := '0';
    signal s_in3    : unsigned(7 downto 0) := (others => '0');
    signal s_out1   : std_ulogic;
    signal s_out2   : std_ulogic;
    signal s_out3   : unsigned(7 downto 0) := (others => '0');
begin
    inst: entity work.test
        port map (
            i_clk    => s_clk,
            i_data1  => s_in1,
            o_data1  => s_out1,
            i_data2  => s_in2,
            o_data2  => s_out2,
            i_data3  => s_in3,
            o_data3  => s_out3
        );

    process
    begin
        s_clk <= '0';
        s_in1 <= '1';
        s_in2 <= '0';
        s_in3 <= "00010100";
        wait for 10 ns;

        wait for 10 ns;
        assert s_out1 = 'U' severity failure;
        assert s_out2 = 'U' severity failure;
        assert std_ulogic_vector(s_out3) = "UUUUUUUU" severity failure;

        s_clk <= '1';
        wait for 10 ns;
        assert s_out1 = 'U' severity failure;
        assert s_out2 = 'U' severity failure;
        assert std_ulogic_vector(s_out3) = "UUUUUUUU" severity failure;

        s_clk <= '0';
        wait for 10 ns;
        s_clk <= '1';
        wait for 10 ns;
        assert s_out1 = '1' severity failure;
        assert s_out2 = '0' severity failure;
        assert std_ulogic_vector(s_out3) = "00010100" severity failure;

        wait;
    end process;
end architecture;

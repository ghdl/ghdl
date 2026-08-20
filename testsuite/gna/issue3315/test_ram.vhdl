library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ram_8x8 is new work.ram_pkg
    generic map (N => 8, A => 3);

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ram_8x8.all;

entity testbench is
    generic(
        LOG_EN : boolean := true
    );
end testbench;

architecture sim of testbench is
    constant CLK_HALF_PER : time := 500 ns;

    signal t_din, t_dout : ram_word  := (others => '0');
    signal t_addr        : addr_word := (others => '0');
    signal t_we, t_clk   : std_logic := '0';

    signal rom_8x8 : memory;

begin

    dut_8x8 : entity work.ram(behav)
		generic map (LOG_EN => LOG_EN, A=>3, N=>8, local_pkg=>work.ram_8x8)
        port map (
            i_din  => t_din,
            o_dout => t_dout,
            i_we   => t_we,
            i_addr => t_addr,
            i_clk  => t_clk,
            i_rst  => '0'
        );

    master_clock : process
    begin
        while true loop
            wait for CLK_HALF_PER;
            wait for CLK_HALF_PER;
        end loop;
    end process;

    sim : process

    begin
        report "*-------------------------*";

		wait for 1 us;

        report "TESTS FINISHED" severity failure;
    end process;

end architecture;

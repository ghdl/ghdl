library ieee;
use ieee.std_logic_1164.all;

use work.abc;
use work.abc.all;

use std.env.finish;

entity abc_tb is
end entity abc_tb;

architecture sim of abc_tb is
    constant CLK_PERIOD : time := 10 ns;

    signal clk : std_ulogic := '0';

    constant abc_BUS_SETTINGS : abc.Parameters_t := (
        BW    => 8,
        PAIRS => 2
    );

    signal abc_bus : abc.Bus_t(
        Indices(abc_BUS_SETTINGS.PAIRS - 1 downto 0)(abc_BUS_SETTINGS.BW - 1 downto 0)
    );

begin
    clk <= not clk after CLK_PERIOD / 2;
    test_runner : process
    begin
		abc_bus <= abc.Test(
			abc_bus,
			(
				0 => std_logic_vector'(x"00"),
				1 => std_logic_vector'(x"01")
			)
		);
		wait for CLK_PERIOD;
		finish;
    end process test_runner;

end architecture sim;

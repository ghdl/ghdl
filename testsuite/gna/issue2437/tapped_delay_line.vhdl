library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package common is
    type signed_array is array(natural range <>) of signed;
end package;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.common.all;

entity tapped_delay_line is
    generic (
        RESET_ACTIVE_LEVEL: std_logic := '0'; -- active low
        REGISTER_FIRST_STAGE: boolean := true;
        NUM_TAPS: positive := 1;
        DATA_WIDTH: positive := 8
    );
    port (
        Clock: in std_logic;
        Reset: in std_logic;
        Enable: in std_logic;

        Data: in signed(DATA_WIDTH-1 downto 0);
        Taps: out signed_array(0 to NUM_TAPS-1)(DATA_WIDTH-1 downto 0)
    );
end entity;

architecture behavioural of tapped_delay_line is
begin
    process(Clock)
    begin
        if Enable = '1' and Reset = RESET_ACTIVE_LEVEL then
            if rising_edge(Clock) then
                for i in 2 to NUM_TAPS-1 loop
                    Taps(i) <= Taps(i-1);
                end loop;

                Taps(1) <= Data;
            end if;
        else
            for i in 1 to NUM_TAPS-1 loop
                Taps(i) <= to_signed(0, DATA_WIDTH);
            end loop;
        end if;
    end process;

end behavioural;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.env.finish;

library work;
use work.common.all;

entity tapped_delay_line_tb is
end entity;

architecture sim of tapped_delay_line_tb is
    constant CLK_PERIOD: time := 0.5 ns;
    constant SIM_DURATION: time := 5 ns;
    constant RESET_ACTIVE_LEVEL: std_logic := '0';

    constant DATA_LEN: positive := 8;
    constant NB_TAPS: positive := 4;

    signal clk, rst: std_logic := '1';

    signal dat: signed(DATA_LEN-1 downto 0) := to_signed(0, DATA_LEN);
    signal taps: signed_array(0 to NB_TAPS-1)(DATA_LEN-1 downto 0);
begin
    tdl: entity work.tapped_delay_line
        generic map(
            RESET_ACTIVE_LEVEL => '0',
            REGISTER_FIRST_STAGE => true,
            NUM_TAPS => NB_TAPS,
            DATA_WIDTH => DATA_LEN
        )
        port map(
            Clock => clk,
            Reset => rst,
            Enable => not rst,
            Data => dat
            -- Taps => taps
        );

    data0: process(clk)
    begin
        if rst = RESET_ACTIVE_LEVEL and rising_edge(clk) then
            dat <= dat + 1;
        end if;
    end process;

    rst0: process
    begin
        rst <= not RESET_ACTIVE_LEVEL;
        wait for CLK_PERIOD*3;
        rst <= RESET_ACTIVE_LEVEL;
        wait;
    end process;

    clock0: process
    begin
        clk <= not clk;
        wait for CLK_PERIOD/2;
    end process;

    sim0: process
    begin
        report "Process started...";
        wait for SIM_DURATION;
        report "Waiting ended, finishing...";
        finish;
    end process;

end sim;

--------------------------------------------------------------------------------
-- PROJECT: SIMPLE UART FOR FPGA
--------------------------------------------------------------------------------
-- AUTHORS: Jakub Cabal <jakubcabal@gmail.com>
-- LICENSE: The MIT License, please read LICENSE file
-- WEBSITE: https://github.com/jakubcabal/uart-for-fpga
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity UART_DEBOUNCER is
    Generic (
        -- latency of debouncer in clock cycles, minimum value is 2,
        -- value also corresponds to the number of bits compared
        LATENCY : natural := 4
    );
    Port (
        CLK     : in  std_logic; -- system clock
        DEB_IN  : in  std_logic; -- input of signal from outside FPGA
        DEB_OUT : out std_logic  -- output of debounced (filtered) signal
    );
end entity;

architecture RTL of UART_DEBOUNCER is

    constant SHREG_DEPTH : natural := LATENCY-1;

    signal input_shreg    : std_logic_vector(SHREG_DEPTH-1 downto 0);
    signal output_reg_rst : std_logic;
    signal output_reg_set : std_logic;

begin

    -- parameterized input shift register
    input_shreg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            input_shreg <= input_shreg(SHREG_DEPTH-2 downto 0) & DEB_IN;
        end if;
    end process;

    -- output register will be reset when all compared bits are low
    output_reg_rst_p : process (DEB_IN, input_shreg)
        variable or_var : std_logic;
    begin
        or_var := DEB_IN;
        all_bits_or_l : for i in 0 to SHREG_DEPTH-1 loop
            or_var := or_var or input_shreg(i);
        end loop;
        output_reg_rst <= not or_var;
    end process;

    -- output register will be set when all compared bits are high
    output_reg_set_p : process (DEB_IN, input_shreg)
        variable and_var : std_logic;
    begin
        and_var := DEB_IN;
        all_bits_and_l : for i in 0 to SHREG_DEPTH-1 loop
            and_var := and_var and input_shreg(i);
        end loop;
        output_reg_set <= and_var;
    end process;

    -- output register
    output_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (output_reg_rst = '1') then
                DEB_OUT <= '0';
            elsif (output_reg_set = '1') then
                DEB_OUT <= '1';
            end if;
        end if;
    end process;

end architecture;

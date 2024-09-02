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
use IEEE.MATH_REAL.ALL;

entity UART_CLK_DIV is
    Generic (
        DIV_MAX_VAL  : integer := 16;
        DIV_MARK_POS : integer := 1
    );
    Port (
        CLK      : in  std_logic; -- system clock
        RST      : in  std_logic; -- high active synchronous reset
        -- USER INTERFACE
        CLEAR    : in  std_logic; -- clock divider counter clear
        ENABLE   : in  std_logic; -- clock divider counter enable
        DIV_MARK : out std_logic  -- output divider mark (divided clock enable)
    );
end entity;

architecture RTL of UART_CLK_DIV is

    constant CLK_DIV_WIDTH  : integer := integer(ceil(log2(real(DIV_MAX_VAL))));

    signal clk_div_cnt      : unsigned(CLK_DIV_WIDTH-1 downto 0);
    signal clk_div_cnt_mark : std_logic;

begin

    clk_div_cnt_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (CLEAR = '1') then
                clk_div_cnt <= (others => '0');
            elsif (ENABLE = '1') then
                if (clk_div_cnt = DIV_MAX_VAL-1) then
                    clk_div_cnt <= (others => '0');
                else
                    clk_div_cnt <= clk_div_cnt + 1;
                end if;
            end if;
        end if;
    end process;

    clk_div_cnt_mark <= '1' when (clk_div_cnt = DIV_MARK_POS) else '0';

    div_mark_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            DIV_MARK <= ENABLE and clk_div_cnt_mark;
        end if;
    end process;

end architecture;

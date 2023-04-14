library ieee;
    use ieee.std_logic_1164.all;

entity test is
    generic (
        BAUD_MULT   : positive := 16
    );
    port (
        clk, rst    : in std_logic;

        brk         : out std_logic;
        rx          : in std_logic
    );
end test;

architecture behavioral of test is

    signal rx_buf   : std_logic := '1';
    signal break    : std_logic := '0';

begin

    BREAK_DETECTOR: process (clk, rst) is
        constant BREAK_CNT : positive := BAUD_MULT * 11;
        variable count : natural range 0 to BREAK_CNT + 1 := 0;
    begin
        if (rising_edge(clk)) then
            rx_buf      <= rx;
            -- Add to counter if '0', but halt count when break detected
            count := (count + 1) when not(rx_buf or break);
            -- Reset counter if '1'
            count := 0 when rx_buf; 

            break <= '0' when (count < BREAK_CNT) else '1';

            if (rst = '1') then
                count := 0;
                break <= '0';
            end if;
        end if;
    end process;

    brk <= break;

end behavioral;
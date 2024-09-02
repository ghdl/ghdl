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

entity UART_RX is
    Generic (
        CLK_DIV_VAL : integer := 16;
        PARITY_BIT  : string  := "none" -- type of parity: "none", "even", "odd", "mark", "space"
    );
    Port (
        CLK          : in  std_logic; -- system clock
        RST          : in  std_logic; -- high active synchronous reset
        -- UART INTERFACE
        UART_CLK_EN  : in  std_logic; -- oversampling (16x) UART clock enable
        UART_RXD     : in  std_logic; -- serial receive data
        -- USER DATA OUTPUT INTERFACE
        DOUT         : out std_logic_vector(7 downto 0); -- output data received via UART
        DOUT_VLD     : out std_logic; -- when DOUT_VLD = 1, output data (DOUT) are valid without errors (is assert only for one clock cycle)
        FRAME_ERROR  : out std_logic; -- when FRAME_ERROR = 1, stop bit was invalid (is assert only for one clock cycle)
        PARITY_ERROR : out std_logic  -- when PARITY_ERROR = 1, parity bit was invalid (is assert only for one clock cycle)
    );
end entity;

architecture RTL of UART_RX is

    signal rx_clk_en          : std_logic;
    signal rx_data            : std_logic_vector(7 downto 0);
    signal rx_bit_count       : unsigned(2 downto 0);
    signal rx_parity_bit      : std_logic;
    signal rx_parity_error    : std_logic;
    signal rx_parity_check_en : std_logic;
    signal rx_done            : std_logic;
    signal fsm_idle           : std_logic;
    signal fsm_databits       : std_logic;
    signal fsm_stopbit        : std_logic;

    type state is (idle, startbit, databits, paritybit, stopbit);
    signal fsm_pstate : state;
    signal fsm_nstate : state;

begin

    -- -------------------------------------------------------------------------
    -- UART RECEIVER CLOCK DIVIDER AND CLOCK ENABLE FLAG
    -- -------------------------------------------------------------------------

    rx_clk_divider_i : entity work.UART_CLK_DIV
    generic map(
        DIV_MAX_VAL  => CLK_DIV_VAL,
        DIV_MARK_POS => 3
    )
    port map (
        CLK      => CLK,
        RST      => RST,
        CLEAR    => fsm_idle,
        ENABLE   => UART_CLK_EN,
        DIV_MARK => rx_clk_en
    );

    -- -------------------------------------------------------------------------
    -- UART RECEIVER BIT COUNTER
    -- -------------------------------------------------------------------------

    uart_rx_bit_counter_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                rx_bit_count <= (others => '0');
            elsif (rx_clk_en = '1' AND fsm_databits = '1') then
                if (rx_bit_count = "111") then
                    rx_bit_count <= (others => '0');
                else
                    rx_bit_count <= rx_bit_count + 1;
                end if;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- UART RECEIVER DATA SHIFT REGISTER
    -- -------------------------------------------------------------------------

    uart_rx_data_shift_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (rx_clk_en = '1' AND fsm_databits = '1') then
                rx_data <= UART_RXD & rx_data(7 downto 1);
            end if;
        end if;
    end process;

    DOUT <= rx_data;

    -- -------------------------------------------------------------------------
    -- UART RECEIVER PARITY GENERATOR AND CHECK
    -- -------------------------------------------------------------------------

    uart_rx_parity_g : if (PARITY_BIT /= "none") generate
        uart_rx_parity_gen_i: entity work.UART_PARITY
        generic map (
            DATA_WIDTH  => 8,
            PARITY_TYPE => PARITY_BIT
        )
        port map (
            DATA_IN     => rx_data,
            PARITY_OUT  => rx_parity_bit
        );

        uart_rx_parity_check_reg_p : process (CLK)
        begin
            if (rising_edge(CLK)) then
                if (rx_clk_en = '1') then
                    rx_parity_error <= rx_parity_bit XOR UART_RXD;
                end if;
            end if;
        end process;
    end generate;

    uart_rx_noparity_g : if (PARITY_BIT = "none") generate
        rx_parity_error <= '0';
    end generate;

    -- -------------------------------------------------------------------------
    -- UART RECEIVER OUTPUT REGISTER
    -- -------------------------------------------------------------------------

    rx_done <= rx_clk_en and fsm_stopbit;

    uart_rx_output_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                DOUT_VLD     <= '0';
                FRAME_ERROR  <= '0';
                PARITY_ERROR <= '0';
            else
                DOUT_VLD     <= rx_done and not rx_parity_error and UART_RXD;
                FRAME_ERROR  <= rx_done and not UART_RXD;
                PARITY_ERROR <= rx_done and rx_parity_error;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- UART RECEIVER FSM
    -- -------------------------------------------------------------------------

    -- PRESENT STATE REGISTER
    process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                fsm_pstate <= idle;
            else
                fsm_pstate <= fsm_nstate;
            end if;
        end if;
    end process;

    -- NEXT STATE AND OUTPUTS LOGIC
    process (fsm_pstate, UART_RXD, rx_clk_en, rx_bit_count)
    begin
        case fsm_pstate is

            when idle =>
                fsm_stopbit  <= '0';
                fsm_databits <= '0';
                fsm_idle     <= '1';

                if (UART_RXD = '0') then
                    fsm_nstate <= startbit;
                else
                    fsm_nstate <= idle;
                end if;

            when startbit =>
                fsm_stopbit  <= '0';
                fsm_databits <= '0';
                fsm_idle     <= '0';

                if (rx_clk_en = '1') then
                    fsm_nstate <= databits;
                else
                    fsm_nstate <= startbit;
                end if;

            when databits =>
                fsm_stopbit  <= '0';
                fsm_databits <= '1';
                fsm_idle     <= '0';

                if ((rx_clk_en = '1') AND (rx_bit_count = "111")) then
                    if (PARITY_BIT = "none") then
                        fsm_nstate <= stopbit;
                    else
                        fsm_nstate <= paritybit;
                    end if ;
                else
                    fsm_nstate <= databits;
                end if;

            when paritybit =>
                fsm_stopbit  <= '0';
                fsm_databits <= '0';
                fsm_idle     <= '0';

                if (rx_clk_en = '1') then
                    fsm_nstate <= stopbit;
                else
                    fsm_nstate <= paritybit;
                end if;

            when stopbit =>
                fsm_stopbit  <= '1';
                fsm_databits <= '0';
                fsm_idle     <= '0';

                if (rx_clk_en = '1') then
                    fsm_nstate <= idle;
                else
                    fsm_nstate <= stopbit;
                end if;

            when others =>
                fsm_stopbit  <= '0';
                fsm_databits <= '0';
                fsm_idle     <= '0';
                fsm_nstate   <= idle;

        end case;
    end process;

end architecture;

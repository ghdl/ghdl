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

entity UART_TX is
    Generic (
        CLK_DIV_VAL : integer := 16;
        PARITY_BIT  : string  := "none" -- type of parity: "none", "even", "odd", "mark", "space"
    );
    Port (
        CLK         : in  std_logic; -- system clock
        RST         : in  std_logic; -- high active synchronous reset
        -- UART INTERFACE
        UART_CLK_EN : in  std_logic; -- oversampling (16x) UART clock enable
        UART_TXD    : out std_logic; -- serial transmit data
        -- USER DATA INPUT INTERFACE
        DIN         : in  std_logic_vector(7 downto 0); -- input data to be transmitted over UART
        DIN_VLD     : in  std_logic; -- when DIN_VLD = 1, input data (DIN) are valid
        DIN_RDY     : out std_logic  -- when DIN_RDY = 1, transmitter is ready and valid input data will be accepted for transmiting
    );
end entity;

architecture RTL of UART_TX is

    signal tx_clk_en       : std_logic;
    signal tx_clk_div_clr  : std_logic;
    signal tx_data         : std_logic_vector(7 downto 0);
    signal tx_bit_count    : unsigned(2 downto 0);
    signal tx_bit_count_en : std_logic;
    signal tx_ready        : std_logic;
    signal tx_parity_bit   : std_logic;
    signal tx_data_out_sel : std_logic_vector(1 downto 0);

    type state is (idle, txsync, startbit, databits, paritybit, stopbit);
    signal tx_pstate : state;
    signal tx_nstate : state;

begin

    DIN_RDY <= tx_ready;

    -- -------------------------------------------------------------------------
    -- UART TRANSMITTER CLOCK DIVIDER AND CLOCK ENABLE FLAG
    -- -------------------------------------------------------------------------

    tx_clk_divider_i : entity work.UART_CLK_DIV
    generic map(
        DIV_MAX_VAL  => CLK_DIV_VAL,
        DIV_MARK_POS => 1
    )
    port map (
        CLK      => CLK,
        RST      => RST,
        CLEAR    => tx_clk_div_clr,
        ENABLE   => UART_CLK_EN,
        DIV_MARK => tx_clk_en
    );

    -- -------------------------------------------------------------------------
    -- UART TRANSMITTER INPUT DATA REGISTER
    -- -------------------------------------------------------------------------

    uart_tx_input_data_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (DIN_VLD = '1' AND tx_ready = '1') then
                tx_data <= DIN;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- UART TRANSMITTER BIT COUNTER
    -- -------------------------------------------------------------------------

    uart_tx_bit_counter_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                tx_bit_count <= (others => '0');
            elsif (tx_bit_count_en = '1' AND tx_clk_en = '1') then
                if (tx_bit_count = "111") then
                    tx_bit_count <= (others => '0');
                else
                    tx_bit_count <= tx_bit_count + 1;
                end if;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- UART TRANSMITTER PARITY GENERATOR
    -- -------------------------------------------------------------------------

    uart_tx_parity_g : if (PARITY_BIT /= "none") generate
        uart_tx_parity_gen_i: entity work.UART_PARITY
        generic map (
            DATA_WIDTH  => 8,
            PARITY_TYPE => PARITY_BIT
        )
        port map (
            DATA_IN     => tx_data,
            PARITY_OUT  => tx_parity_bit
        );
    end generate;

    uart_tx_noparity_g : if (PARITY_BIT = "none") generate
        tx_parity_bit <= '0';
    end generate;

    -- -------------------------------------------------------------------------
    -- UART TRANSMITTER OUTPUT DATA REGISTER
    -- -------------------------------------------------------------------------

    uart_tx_output_data_reg_p : process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                UART_TXD <= '1';
            else
                case tx_data_out_sel is
                    when "01" => -- START BIT
                        UART_TXD <= '0';
                    when "10" => -- DATA BITS
                        UART_TXD <= tx_data(to_integer(tx_bit_count));
                    when "11" => -- PARITY BIT
                        UART_TXD <= tx_parity_bit;
                    when others => -- STOP BIT OR IDLE
                        UART_TXD <= '1';
                end case;
            end if;
        end if;
    end process;

    -- -------------------------------------------------------------------------
    -- UART TRANSMITTER FSM
    -- -------------------------------------------------------------------------

    -- PRESENT STATE REGISTER
    process (CLK)
    begin
        if (rising_edge(CLK)) then
            if (RST = '1') then
                tx_pstate <= idle;
            else
                tx_pstate <= tx_nstate;
            end if;
        end if;
    end process;

    -- NEXT STATE AND OUTPUTS LOGIC
    process (tx_pstate, DIN_VLD, tx_clk_en, tx_bit_count)
    begin

        case tx_pstate is

            when idle =>
                tx_ready <= '1';
                tx_data_out_sel <= "00";
                tx_bit_count_en <= '0';
                tx_clk_div_clr <= '1';

                if (DIN_VLD = '1') then
                    tx_nstate <= txsync;
                else
                    tx_nstate <= idle;
                end if;

            when txsync =>
                tx_ready <= '0';
                tx_data_out_sel <= "00";
                tx_bit_count_en <= '0';
                tx_clk_div_clr <= '0';

                if (tx_clk_en = '1') then
                    tx_nstate <= startbit;
                else
                    tx_nstate <= txsync;
                end if;

            when startbit =>
                tx_ready <= '0';
                tx_data_out_sel <= "01";
                tx_bit_count_en <= '0';
                tx_clk_div_clr <= '0';

                if (tx_clk_en = '1') then
                    tx_nstate <= databits;
                else
                    tx_nstate <= startbit;
                end if;

            when databits =>
                tx_ready <= '0';
                tx_data_out_sel <= "10";
                tx_bit_count_en <= '1';
                tx_clk_div_clr <= '0';

                if ((tx_clk_en = '1') AND (tx_bit_count = "111")) then
                    if (PARITY_BIT = "none") then
                        tx_nstate <= stopbit;
                    else
                        tx_nstate <= paritybit;
                    end if ;
                else
                    tx_nstate <= databits;
                end if;

            when paritybit =>
                tx_ready <= '0';
                tx_data_out_sel <= "11";
                tx_bit_count_en <= '0';
                tx_clk_div_clr <= '0';

                if (tx_clk_en = '1') then
                    tx_nstate <= stopbit;
                else
                    tx_nstate <= paritybit;
                end if;

            when stopbit =>
                tx_ready <= '1';
                tx_data_out_sel <= "00";
                tx_bit_count_en <= '0';
                tx_clk_div_clr <= '0';

                if (DIN_VLD = '1') then
                    tx_nstate <= txsync;
                elsif (tx_clk_en = '1') then
                    tx_nstate <= idle;
                else
                    tx_nstate <= stopbit;
                end if;

            when others =>
                tx_ready <= '0';
                tx_data_out_sel <= "00";
                tx_bit_count_en <= '0';
                tx_clk_div_clr <= '0';
                tx_nstate <= idle;

        end case;
    end process;

end architecture;

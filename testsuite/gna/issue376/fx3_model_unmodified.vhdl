-- Copyright (c) 2013 Nuand LLC
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;
    use ieee.math_real.all ;
    use ieee.math_complex.all ;

library nuand ;
    use nuand.util.all ;

entity fx3_model is
  port (
    fx3_pclk            :   buffer  std_logic := '1' ;
    fx3_gpif            :   inout   std_logic_vector(31 downto 0) ;
    fx3_ctl             :   inout   std_logic_vector(12 downto 0) ;
    fx3_uart_rxd        :   in      std_logic ;
    fx3_uart_txd        :   buffer  std_logic ;
    fx3_uart_cts        :   buffer  std_logic ;
    fx3_rx_en           :   in      std_logic ;
    fx3_rx_meta_en      :   in      std_logic ;
    fx3_tx_en           :   in      std_logic ;
    fx3_tx_meta_en      :   in      std_logic
  ) ;
end entity ; -- fx3_model

architecture dma of fx3_model is

    constant PCLK_HALF_PERIOD       : time  := 1 sec * (1.0/100.0e6/2.0) ;

    -- Control mapping
    alias dma0_rx_ack   is fx3_ctl( 0) ;
    alias dma1_rx_ack   is fx3_ctl( 1) ;
    alias dma2_tx_ack   is fx3_ctl( 2) ;
    alias dma3_tx_ack   is fx3_ctl( 3) ;
    alias dma_rx_enable is fx3_ctl( 4) ;
    alias dma_tx_enable is fx3_ctl( 5) ;
    alias dma_idle      is fx3_ctl( 6) ;
    alias system_reset  is fx3_ctl( 7) ;
    alias dma0_rx_reqx  is fx3_ctl( 8) ;
    alias dma1_rx_reqx  is fx3_ctl(12) ; -- due to 9 being connected to dclk
    alias dma2_tx_reqx  is fx3_ctl(10) ;
    alias dma3_tx_reqx  is fx3_ctl(11) ;

    type gpif_state_t is (IDLE, TX_SAMPLES, RX_SAMPLES) ;
    signal gpif_state : gpif_state_t ;

begin

    -- DCLK which isn't used
    fx3_ctl(9) <= '0' ;

    -- Create a 100MHz clock output
    fx3_pclk <= not fx3_pclk after PCLK_HALF_PERIOD ;

    rx_sample_stream : process
        constant BLOCK_SIZE     : natural   := 512 ;
        variable count          : natural   := 0 ;
    begin
        dma0_rx_reqx <= '1' ;
        dma1_rx_reqx <= '1' ;
        dma_rx_enable <= '0' ;
        wait until rising_edge(fx3_pclk) and system_reset = '0' ;
        for i in 1 to 10 loop
            wait until rising_edge( fx3_pclk ) ;
        end loop ;
        if( fx3_rx_en = '0' ) then
            wait;
        end if;
        wait for 30 us;
        dma_rx_enable <= '1' ;
        while true loop
            for i in 0 to 2 loop
                dma0_rx_reqx <= '0' ;
                wait until rising_edge( fx3_pclk ) and dma0_rx_ack = '1' ;
                wait until rising_edge( fx3_pclk ) ;
                wait until rising_edge( fx3_pclk ) ;
                dma0_rx_reqx <= '1' ;
                for i in 1 to BLOCK_SIZE loop
                    wait until rising_edge( fx3_pclk ) ;
                end loop ;
            end loop ;
            dma_rx_enable <= '0' ;
            for i in 0 to 5000 loop
                wait until rising_edge(fx3_pclk) ;
            end loop ;
            dma_rx_enable <= '1' ;
            for i in 0 to 10 loop
                wait until rising_edge(fx3_pclk);
            end loop ;
        end loop ;
        report "Done with RX sample stream" ;
        wait ;
    end process ;

    tx_sample_stream : process
        constant BLOCK_SIZE : natural := 512 ;
        variable count : natural := 0 ;
        variable timestamp_cntr : natural := 80;
        variable header_len : natural := 0;
    begin
        dma2_tx_reqx <= '1' ;
        dma3_tx_reqx <= '1' ;
        dma_tx_enable <= '0' ;
        fx3_gpif <= (others =>'Z') ;
        wait until system_reset = '0' ;
        for i in 0 to 1000 loop
            wait until rising_edge( fx3_pclk ) ;
        end loop ;
        if( fx3_tx_en = '0' ) then
            wait;
        end if;
        wait for 120 us;
        dma_tx_enable <= '1' ;
        for i in 0 to 3 loop
            dma3_tx_reqx <= '0' ;
            wait until rising_edge( fx3_pclk ) and dma3_tx_ack = '1' ;
            wait until rising_edge( fx3_pclk ) ;
            wait until rising_edge( fx3_pclk ) ;
            dma3_tx_reqx <= '1' ;
            if( fx3_tx_meta_en = '1') then
                for i in 1 to 4 loop
                    if (i = 1 ) then
                        fx3_gpif <= x"12341234";
                    elsif (i = 3 ) then
                        fx3_gpif <= (others => '0');
                    elsif(i = 4) then
                        fx3_gpif <= (others => '1');
                    elsif (i = 2) then
                        fx3_gpif(31 downto 0) <= std_logic_vector(to_signed(timestamp_cntr, 32));
                        timestamp_cntr := timestamp_cntr + 508 * 2;
                    end if;
                    wait until rising_edge( fx3_pclk );
                end loop;
                header_len := 4;
            else
                header_len := 0;
            end if;
            for i in 1 to BLOCK_SIZE - header_len loop
                fx3_gpif(31 downto 16) <= std_logic_vector(to_signed(count, 16)) ;
                fx3_gpif(15 downto 0) <= std_logic_vector(to_signed(-count, 16)) ;
                count := (count + 1) mod 2048 ;
                wait until rising_edge( fx3_pclk );
            end loop ;
            fx3_gpif <= (others =>'Z');
            for i in 1 to 10 loop
                wait until rising_edge( fx3_pclk );
            end loop ;
       end loop ;
       report "Done with TX sample stream" ;
       wait ;
    end process ;

    reset_system : process
    begin
        system_reset <= '1' ;
        dma_idle <= '0' ;
        nop( fx3_pclk, 100 ) ;
        system_reset <= '0' ;
        nop( fx3_pclk, 10 ) ;
        dma_idle <= '1' ;
        wait ;
    end process ;

    -- TODO: UART Interface
    fx3_uart_txd <= '1' ;
    fx3_uart_cts <= '1' ;

end architecture ; -- dma

architecture inband_scheduler of fx3_model is

begin

end architecture ; -- inband_scheduler

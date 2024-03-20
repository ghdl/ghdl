library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

entity tb_sdcard is
end entity tb_sdcard;

architecture simulation of tb_sdcard is

   signal test_running : std_logic      := '1';

   signal avm_clk           : std_logic := '1'; -- 50 Mhz
   signal avm_rst           : std_logic := '1'; -- Synchronous reset, active high
   signal avm_write         : std_logic;
   signal avm_read          : std_logic;
   signal avm_address       : std_logic_vector(31 downto 0);
   signal avm_writedata     : std_logic_vector(7 downto 0);
   signal avm_burstcount    : std_logic_vector(15 downto 0);
   signal avm_readdata      : std_logic_vector(7 downto 0);
   signal avm_readdatavalid : std_logic;
   signal avm_waitrequest   : std_logic;
   signal avm_init_error    : std_logic;
   signal avm_crc_error     : std_logic;
   signal avm_last_state    : std_logic_vector(7 downto 0);
   signal sd_clk            : std_logic;
   signal sd_cmd            : std_logic;
   signal sd_dat            : std_logic_vector(3 downto 0);

   signal mem_addr     : std_logic_vector(31 downto 0);
   signal mem_data_out : std_logic_vector(7 downto 0);
   signal mem_data_in  : std_logic_vector(7 downto 0);
   signal mem_we       : std_logic;

begin

   ---------------------------------------------------------
   -- Generate clock and reset
   ---------------------------------------------------------

   avm_clk <= test_running and not avm_clk after 10 ns;
   avm_rst <= '1', '0' after 100 ns;


   ---------------------------------------------------------
   -- Main test procedure
   ---------------------------------------------------------

   test_proc : process
   begin
      avm_write <= '0';
      avm_read  <= '0';
      wait until avm_rst = '0';
      wait until rising_edge(avm_clk);
      wait until rising_edge(avm_clk);

      report "Test finished";
      test_running <= '0';
      wait;
   end process test_proc;


   ---------------------------------------------------------
   -- Instantiate SDCard controller
   ---------------------------------------------------------

   sdcard_wrapper_inst : entity work.sdcard_wrapper
      generic map (
         G_UART => false
      )
      port map (
         avm_clk_i           => avm_clk,
         avm_rst_i           => avm_rst,
         avm_write_i         => avm_write,
         avm_read_i          => avm_read,
         avm_address_i       => avm_address,
         avm_writedata_i     => avm_writedata,
         avm_burstcount_i    => avm_burstcount,
         avm_readdata_o      => avm_readdata,
         avm_readdatavalid_o => avm_readdatavalid,
         avm_waitrequest_o   => avm_waitrequest,
         avm_init_error_o    => avm_init_error,
         avm_crc_error_o     => avm_crc_error,
         avm_last_state_o    => avm_last_state,
         sd_cd_i             => '1',
         sd_clk_o            => sd_clk,
         sd_cmd_io           => sd_cmd,
         sd_dat_io           => sd_dat,
         uart_valid_o        => open,
         uart_ready_i        => '1',
         uart_data_o         => open
      ); -- sdcard_wrapper_inst


   ---------------------------------------------------------
   -- Instantiate SDCard simulation model
   ---------------------------------------------------------

   sdcard_sim_inst : entity work.sdcard_sim
      port map (
         sd_clk_i   => sd_clk,
         sd_cmd_io  => sd_cmd,
         sd_dat_io  => sd_dat,
         mem_addr_o => mem_addr,
         mem_data_o => mem_data_out,
         mem_data_i => mem_data_in,
         mem_we_o   => mem_we
      ); -- sdcard_sim_inst

end architecture simulation;


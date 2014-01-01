-------------------------------------------------------------------------------
-- Master testbench version 3
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity master_testbench3 is
  -- Nada !
end master_testbench3;


use work.polyamplib.all;

architecture master_testbench3_arch of master_testbench3 is

  shared variable running : boolean := true;  -- Run testbench when true

  -- Global signals
  signal fastclk              : std_logic := '0';
  signal resync               : std_logic := '0';
  signal adc_data_0           : std_logic_vector(23 downto 0);  -- Bus from AD-interface
  signal adc_data_1           : std_logic_vector(23 downto 0);  -- to mux
  signal fifo_write           : std_logic;
  signal fifo_input           : std_logic_vector(31 downto 0);
  signal fifo_output          : std_logic_vector(31 downto 0);
  signal id_code              : unsigned(3 downto 0);
  signal id_code_i            : unsigned(3 downto 0);
  signal fifo_isempty         : std_logic;
  signal fifo_meter           : unsigned(7 downto 0);
  signal fifo_half_full       : std_logic := '0';
  signal enable_adcvalues     : std_logic := '0';  -- Enable automatic emission of values
  signal not_enable_adcvalues : std_logic;

  -- Clocks
  signal clk24M : std_logic := '0';
  signal clk4M  : std_logic := '0';
  signal clk2M  : std_logic := '0';


  -- Channel 0
  signal advalue_0     : signed(23 downto 0) := to_signed(192, 24);  --Input to ADC
  signal adclk_0       : std_logic;
  signal ad_ser_data_0 : std_logic;
  signal n_drdy_0      : std_logic;
  signal n_sync_0      : std_logic;
  signal adi_busy_0    : std_logic;

  -- Channel 1
  signal advalue_1     : signed(23 downto 0) := to_signed(193, 24);
  signal adclk_1       : std_logic;
  signal ad_ser_data_1 : std_logic;
  signal n_drdy_1      : std_logic;
  signal n_sync_1      : std_logic;
  signal adi_busy_1    : std_logic;

  -- Spi interface
  signal addr_data  : std_logic_vector(31 downto 0);  -- Received command/data
  signal exec_cmd   : std_logic := '0';
  signal fifo_read  : std_logic := '0';
  signal start_adcs : std_logic := '0';
  signal stop_adcs  : std_logic := '0';
  signal mosi       : std_logic := '0';
  signal miso       : std_logic;
  signal sck        : std_logic := '1';
  signal en_adc     : std_logic := '1';               -- Active low
  signal en_incl    : std_logic := '1';               -- Active low
  signal incl_miso  : std_logic := '0';
  signal incl_mosi  : std_logic;
  signal incl_sck   : std_logic;
  signal incl_ena   : std_logic;

  -- Simulation
  signal data_to_spi    : std_logic_vector(31 downto 0);
  signal data_from_spi  : std_logic_vector(31 downto 0);
  signal start          : std_logic := '0';
  signal spim_busy      : std_logic := '0';
  signal signal_running : std_logic := '1';

  
begin  -- master_testbench3_arch


  -----------------------------------------------------------------------------
  -- Component instantiations
  -----------------------------------------------------------------------------

  adc0 : ads1271_model port map (
    analog_in => advalue_0,
    clk       => adclk_0,
    sclk      => adclk_0,
    n_sync    => n_sync_0,
    din       => '0',
    dout      => ad_ser_data_0,
    n_drdy    => n_drdy_0);

  adc1 : ads1271_model port map (
    analog_in => advalue_1,
    clk       => adclk_1,
    sclk      => adclk_1,
    n_sync    => n_sync_1,
    din       => '0',
    dout      => ad_ser_data_1,
    n_drdy    => n_drdy_1);

  adi0 : interface_ads1271 port map (
    fastclk  => fastclk,
    adclk    => adclk_0,
    resync   => resync,
    ser_data => ad_ser_data_0,
    n_drdy   => n_drdy_0,
    data_out => adc_data_0,
    nsync    => n_sync_0,
    busy     => adi_busy_0);

  adi1 : interface_ads1271 port map (
    fastclk  => fastclk,
    adclk    => adclk_1,
    resync   => resync,
    ser_data => ad_ser_data_1,
    n_drdy   => n_drdy_1,
    data_out => adc_data_1,
    nsync    => n_sync_1,
    busy     => adi_busy_1);

  msm : master_state_machine port map (
    fastclk                 => fastclk,
    enable_lo8              => enable_adcvalues,
    busy_n_vec(0)           => adi_busy_0,
    busy_n_vec(1)           => adi_busy_1,
    busy_n_vec(11 downto 2) => "1111111110",  -- Not used in testbench
    id_code                 => id_code,
    fifo_write              => fifo_write);

  adck0 : clockmux port map (
    clk24M  => clk24M,
    clk4M   => clk4M,
    clk2M   => clk2M,
    clk1M   => '0',
    clk512k => '0',
    clk256k => '0',
    clk128k => '0',
    sel     => "110",                   -- 24 MHz clock => 48 ksa/s
    clkout  => adclk_0);

  adck1 : clockmux port map (
    clk24M  => clk24M,
    clk4M   => clk4M,
    clk2M   => clk2M,
    clk1M   => '0',
    clk512k => '0',
    clk256k => '0',
    clk128k => '0',
    sel     => "100",                   -- 2 MHz clock => 4 ksa/s
    clkout  => adclk_1);

  fifo : fifo_ft_memory port map (
    clock   => fastclk,
    sclr    => not_enable_adcvalues,    -- fifo_sclr @@@@@@@@@@
    datain  => fifo_input,
    wrreq   => fifo_write,
    rdreq   => fifo_read,
    dataout => fifo_output,
    empty   => fifo_isempty,
    meter   => fifo_meter);

  sync : sync_logic_2 port map (
    start_adcs       => start_adcs,
    stop_adcs        => stop_adcs,
    reset            => '0',
    hwsync           => '0',
    fastclk          => fastclk,
    enable_adcvalues => enable_adcvalues);

  spis : spi_slave_burst port map (
    fastclk   => fastclk,
    spi_tx    => fifo_output,
    spi_rx    => addr_data,
--    spi_op    => spi_op,
    exec_cmd  => exec_cmd,
    fifo_read => fifo_read,
    mosi      => mosi,
    miso      => miso,
    sck       => sck,
    en_adc    => en_adc,
    en_incl   => en_incl,
    incl_miso => incl_miso,
    incl_mosi => incl_mosi,
    incl_sck  => incl_sck,
    incl_ena  => incl_ena);

  cmd : command_decoder port map (
    addr_data  => addr_data,
    decode     => exec_cmd,
    fastclk    => fastclk,
    resync_adc => resync,
    start_adcs => start_adcs,
    stop_adcs  => stop_adcs);
--    empty_fifo => fifo_sclr);           -- fifo_sclr / open

  spim : spi_master port map (
    miso          => miso,
    mosi          => mosi,
    sck           => sck,
    en_adval      => en_adc,
    en_incl       => en_incl,
    data_to_spi   => data_to_spi,
    data_from_spi => data_from_spi,
    start         => start,
    busy          => spim_busy,
    running       => signal_running);


  -----------------------------------------------------------------------------
  -- Here comes the code
  -----------------------------------------------------------------------------

  id_code_i            <= id_code + 1;    -- Skip NULL command/address
  fifo_half_full       <= fifo_meter(6);  -- >= 64
  not_enable_adcvalues <= not enable_adcvalues;

  with id_code select
    fifo_input(23 downto 0) <=
    adc_data_0                 when "0000",
    adc_data_1                 when "0001",
    "000000000000000000000000" when others;
  fifo_input(27 downto 24) <= "0000" when fifo_isempty = '1' else std_logic_vector(id_code_i);
  fifo_input(31 downto 28) <= "0000";


  -----------------------------------------------------------------------------

  fetch_data : process
  begin  -- process fetch_data
    data_to_spi <= "00000001000000000000000000000100";  -- sel_adclk0
    wait for 2500 ns;
    start       <= '1';
    wait until spim_busy = '1';
    start       <= '0';
    wait until spim_busy = '0';
    wait for 1 us;

    data_to_spi <= "00000010000000000000000000000100";  -- sel_adclk1
    wait for 7 us;
    start       <= '1';
    wait until spim_busy = '1';
    start       <= '0';
    wait until spim_busy = '0';
    wait for 1 us;

--    data_to_spi <= "00000000000000000000000000000000";  -- Null command
    data_to_spi <= "00001001000000000000000000000000";  -- resync
--    wait for 15 us;
    wait for 1 us;
    start       <= '1';
    wait until spim_busy = '1';
    start       <= '0';
    wait until spim_busy = '0';
    wait for 1 us;

    data_to_spi <= "00001011000000000000000000000000";  -- Start AD-conv.
--    wait for 15 us;
    wait for 1 us;
    start       <= '1';
    wait until spim_busy = '1';
    start       <= '0';
    wait until spim_busy = '0';
    wait for 1 us;

    for j in 1 to 250 loop
      data_to_spi <= "00000000000000000000000000000000";  -- Null command
--      wait for 15 us;
      wait for 2 us;
      start       <= '1';
      wait until spim_busy = '1';
      start       <= '0';
      wait until spim_busy = '0';
      wait for 1 us;
    end loop;  -- j

    wait for 2000 us;

    for j in 1 to 250 loop
      data_to_spi <= "00000000000000000000000000000000";  -- Null command
--      wait for 15 us;
      wait for 2 us;
      start       <= '1';
      wait until spim_busy = '1';
      start       <= '0';
      wait until spim_busy = '0';
      wait for 1 us;
    end loop;  -- j

    wait;
  end process fetch_data;

  -----------------------------------------------------------------------------

  main_timing : process
  begin  -- process main_timing
    wait for 6 ms;                   -- Run during this time
    signal_running <= '0';
    running        := false;
    wait;
  end process main_timing;

  -----------------------------------------------------------------------------

  osc_fastclk : process
  begin  -- process osc_fastclk
    while running = true loop
      fastclk <= '0';
      wait for 10173 ps;
      fastclk <= '1';
      wait for 10173 ps;
    end loop;
    wait for 10 ns;
    wait;
  end process osc_fastclk;

  -----------------------------------------------------------------------------

  osc24M : process
  begin  -- process osc24M
    while running = true loop
      clk24M <= '0';
      wait for 20345 ps;
      clk24M <= '1';
      wait for 20345 ps;
    end loop;
    wait for 10 ns;
    wait;
  end process osc24M;

  -----------------------------------------------------------------------------

  osc4M : process
  begin  -- process osc4M
    while running = true loop
      clk4M <= '0';
      wait for 122 ns;
      clk4M <= '1';
      wait for 122 ns;
    end loop;
    wait for 10 ns;
    wait;
  end process osc4M;

  -----------------------------------------------------------------------------

  osc2M : process
  begin  -- process osc2M
    while running = true loop
      clk2M <= '0';
      wait for 244 ns;
      clk2M <= '1';
      wait for 244 ns;
    end loop;
    wait for 10 ns;
    wait;
  end process osc2M;


end master_testbench3_arch;

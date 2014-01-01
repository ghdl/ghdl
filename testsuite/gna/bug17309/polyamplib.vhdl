-----------------------------------------------------------------------------
-- Polyamp vhdl function library
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package polyamplib is

  component interface_ads1271
    port (
      fastclk  : in  std_logic;         -- Fast clock
      adclk    : in  std_logic;         -- A/D converter clock
      resync   : in  std_logic;         -- Clock change; resync
      ser_data : in  std_logic;         -- Serial data in
      n_drdy   : in  std_logic;         -- Data is ready, active low
      data_out : out std_logic_vector(23 downto 0);  -- Parallell data out
      nsync    : out std_logic;         -- Synchronize (restart) A/D conv.
      busy     : out std_logic;         -- Busy reading serial data
      g_sclk   : out std_logic);        -- Gated SPI clock, FOR SIMULATION ONLY
  end component;

  component priority_resolver2 is
    port (
      inp    : in  std_logic_vector(11 downto 0);
      prio   : out unsigned(3 downto 0);  -- Output highest signal
      active : out std_logic);            -- High when any input is active
  end component;

  component one_of_n_encoder is
    port (
      number     : in  unsigned(3 downto 0);
      enable     : in  std_logic;
      single_out : out std_logic_vector(11 downto 0));  -- Only a single wire is active at a time
  end component;

  component active_input is
    port (
      fastclk    : in  std_logic;       -- Main clock
      enable     : in  std_logic;       -- Enable signal
      busy_n     : in  std_logic;       -- Busy input signal, active low
      clear      : in  std_logic;       -- Clear the corresponding flip-flop
      active_out : out std_logic);      -- Active output
  end component;

  component master_state_machine is
    port (
      fastclk    : in    std_logic := '0';               -- Main clock
      enable_lo8 : in    std_logic;     -- Enable the 8 low inputs
      busy_n_vec : in    std_logic_vector(11 downto 0);  -- Busy inputs
      id_code    : inout unsigned(3 downto 0);           -- Id code output
      fifo_write : out   std_logic);    -- Save to fifo
  end component;

  component fifo_memory is
    generic (
      fifo_size : natural := 100);
    port (
      clock   : in  std_logic;
      sclr    : in  std_logic;
      datain  : in  std_logic_vector(31 downto 0);
      wrreq   : in  std_logic;
      rdreq   : in  std_logic;
      dataout : out std_logic_vector(31 downto 0);
      full    : out std_logic;
      empty   : out std_logic;
      meter   : out unsigned(7 downto 0));
  end component;

  component fifo_ft_memory is
    generic (
      fifo_size : natural := 100);
    port (
      clock   : in  std_logic;
      sclr    : in  std_logic;
      datain  : in  std_logic_vector(31 downto 0);
      wrreq   : in  std_logic;
      rdreq   : in  std_logic;
      dataout : out std_logic_vector(31 downto 0);
      full    : out std_logic;
      empty   : out std_logic;
      meter   : out unsigned(7 downto 0));
  end component;

  component ads1271_model is
    port (
      analog_in : in    signed(23 downto 0);  -- Analog input signal
      clk       : in    std_logic;            -- Conversion clock
      sclk      : in    std_logic;            -- SPI clock
      n_sync    : in    std_logic;            -- Input sync signal, active low
      din       : in    std_logic;            -- Serial data in
      dout      : out   std_logic;            -- Serial data out
      n_drdy    : out   std_logic);           -- Data ready, active low
  end component;

  component sr_sipo is
    generic (
      LENGTH : positive := 8;           --! Shift register length
      EDGE   : natural  := 1;           --! Active edge; 1=positive, 0=negative
      DIR    : natural  := 1);          --! Direction; 1=left, 0=right
    port (
      clk     : in  std_logic;          --! Clock input
      ser_in  : in  std_logic;          --! Serial input data
      par_out : out std_logic_vector(1 to LENGTH));  --! Parallel output data
  end component;

  component sr_piso is
    generic (
      LENGTH : positive := 8;           --! Shift register length
      EDGE   : natural  := 1;           --! Active edge; 1=positive, 0=negative
      DIR    : natural  := 1);          --! Direction; 1=left, 0=right
    port (
      clk     : in  std_logic;          --! Clock input
      load    : in  std_logic;          --! Load on next clock
      par_in  : in  std_logic_vector(1 to LENGTH);  --! Parallel input data
      ser_in  : in  std_logic;          --! Serial input data
      ser_out : out std_logic);         --! Serial output data
  end component;

  component sr_piso_s is
    generic (
      LENGTH : positive := 8;           --! Shift register length
      DIR    : natural  := 1);          --! Direction; 1=left, 0=right
    port (
      fastclk : in  std_logic;          --! Synchronous clock
      clk_en  : in  std_logic;          --! Clock enable input
      load_en : in  std_logic;          --! Load enable input
      par_in  : in  std_logic_vector(1 to LENGTH);  --! Parallel input data
      ser_in  : in  std_logic;          --! Serial input data
      ser_out : out std_logic);         --! Serial output data
  end component;

  component spi_slave is
    port (
      -- Main controls
      fastclk : in  std_logic;
      spi_tx  : in  std_logic_vector(31 downto 0);  -- Data to be sent
      spi_rx  : out std_logic_vector(31 downto 0);  -- Data to be received
      spi_op  : out std_logic;          -- Read/Write status/data/command

      -- Slave port, connected to CPU
      mosi    : in  std_logic;
      miso    : out std_logic;
      sck     : in  std_logic;          -- SPI clock
      en_adc  : in  std_logic;          -- Active low, enable ADC
      en_incl : in  std_logic;          -- Active low, enable inclinometer

      -- Master port, connected to inclinometer
      incl_miso : in  std_logic;
      incl_mosi : out std_logic;
      incl_sck  : out std_logic;
      incl_ena  : out std_logic);       -- Active low, enable inclinometer
  end component;

  component command_decoder is
    port (
      addr_data  : in  std_logic_vector(31 downto 0);  -- Input address/data
      decode     : in  std_logic;       -- Single cycle decode pulse
      fastclk    : in  std_logic;       -- Master clock (not used for now)
      sel_nulcmd : out std_logic;       -- NULL command (no operation)
      sel_adclk0 : out std_logic;       -- Select sampling clock, ad0.
      sel_adclk1 : out std_logic;       -- Select sampling clock, ad1.
      sel_adclk2 : out std_logic;       -- Select sampling clock, ad2.
      sel_adclk3 : out std_logic;       -- Select sampling clock, ad3.
      sel_adclk4 : out std_logic;       -- Select sampling clock, ad4.
      sel_adclk5 : out std_logic;       -- Select sampling clock, ad5.
      sel_adclk6 : out std_logic;       -- Select sampling clock, ad6.
      sel_adclk7 : out std_logic;       -- Select sampling clock, ad7.
      resync_adc : out std_logic;       -- Resynchronize all ADC's
      write_ctrl : out std_logic;       -- Write to control-signal register
      start_adcs : out std_logic;       -- Start AD-conversion
      stop_adcs  : out std_logic);      -- Stop AD-conversion
  end component;

  component clockmux is
    port (
      clk24M  : in  std_logic;             -- Input clocks, 24 576 000
      clk4M   : in  std_logic;             -- 4 096 000
      clk2M   : in  std_logic;             -- 2 048 000
      clk1M   : in  std_logic;             -- 1 024 000
      clk512k : in  std_logic;             -- 512 000
      clk256k : in  std_logic;             -- 256 000
      clk128k : in  std_logic;             -- 128 000
      sel     : in  unsigned(2 downto 0);  -- Mux select input
      clkout  : out std_logic);            -- Output clock
  end component;

  component spi_master is
    port (
      -- Hardware ports
      miso     : in    std_logic;
      mosi     : out   std_logic;
      sck      : inout std_logic;
      en_adval : out   std_logic := '1';
      en_incl  : out   std_logic := '1';

      -- Simulation ports
      data_to_spi   : in  std_logic_vector(31 downto 0);
      data_from_spi : out std_logic_vector(31 downto 0);
      start         : in  std_logic;
      busy          : out std_logic := '0';
      running       : in  std_logic);
  end component;

  component spi_slave_burst is
    port (
      -- Main controls
      fastclk   : in  std_logic;
      spi_tx    : in  std_logic_vector(31 downto 0);  -- Data to be sent
      spi_rx    : out std_logic_vector(31 downto 0);  -- Data to be received
      exec_cmd  : out std_logic;                      -- Write command/data
      fifo_read : out std_logic;                      -- Read status/data

      -- Slave port, connected to CPU
      mosi    : in  std_logic;
      miso    : out std_logic;
      sck     : in  std_logic;          -- SPI clock
      en_adc  : in  std_logic;          -- Active low, enable ADC
      en_incl : in  std_logic;          -- Active low, enable inclinometer

      -- Master port, connected to inclinometer
      incl_miso : in  std_logic;
      incl_mosi : out std_logic;
      incl_sck  : out std_logic;
      incl_ena  : out std_logic);       -- Active low, enable inclinometer
  end component;

  component sync_logic_2 is
    port (
      start_adcs       : in  std_logic;   -- Start command
      stop_adcs        : in  std_logic;   -- Stop command
      reset            : in  std_logic;   -- Active high
      hwsync           : in  std_logic;   -- Hardware sync
      fastclk          : in  std_logic;   -- Master clock
      enable_adcvalues : out std_logic);  -- Enable reception of values
  end component;

end polyamplib;


-----------------------------------------------------------------------------
-- Shift register for adc_interface, spi_interface etc.
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


--! General serial input, parallell output shift register

--! The shift register length is generic, from 2 and up. It can trigger on
--! positive or negative edges, and the shift direction can be choosen as well.
entity sr_sipo is
  generic (
    LENGTH : positive := 8;             --! Shift register length
    EDGE   : natural  := 1;             --! Active edge; 1=positive, 0=negative
    DIR    : natural  := 1);            --! Direction; 1=left, 0=right
  port (
    clk     : in  std_logic;            --! Clock input
    ser_in  : in  std_logic;            --! Serial input data
    par_out : out std_logic_vector(1 to LENGTH));  --! Parallel output data
end sr_sipo;

architecture sr_arch of sr_sipo is

  signal my_sr : std_logic_vector(1 to LENGTH);

begin  -- sr_arch
  
  posedge_right : if (EDGE = 1) and (DIR = 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '1' then   -- Positive clock edge
        my_sr(1 to LENGTH) <= (my_sr(2 to LENGTH) & ser_in);
      end if;
    end process shift;
  end generate posedge_right;

  posedge_left : if (EDGE = 1) and (DIR /= 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '1' then   -- Positive clock edge
        my_sr(1 to LENGTH) <= (ser_in & my_sr(1 to LENGTH-1));
      end if;
    end process shift;
  end generate posedge_left;

  negedge_right : if (EDGE /= 1) and (DIR = 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '0' then   -- Negative clock edge
        my_sr(1 to LENGTH) <= (my_sr(2 to LENGTH) & ser_in);
      end if;
    end process shift;
  end generate negedge_right;

  negedge_left : if (EDGE /= 1) and (DIR /= 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '0' then   -- Negative clock edge
        my_sr(1 to LENGTH) <= (ser_in & my_sr(1 to LENGTH-1));
      end if;
    end process shift;
  end generate negedge_left;

  par_out <= my_sr(1 to LENGTH) after 2 ns;

end sr_arch;





-----------------------------------------------------------------------------
-- Shift register for spi_interface
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


--! General parallell input, serial output shift register

--! The shift register length is generic, from 2 and up. It can trigger on
--! positive or negative edges, and the shift direction can be choosen as well.

entity sr_piso is
  generic (
    LENGTH : positive := 8;             --! Shift register length
    EDGE   : natural  := 1;             --! Active edge; 1=positive, 0=negative
    DIR    : natural  := 1);            --! Direction; 1=left, 0=right
  port (
    clk     : in  std_logic;            --! Clock input
    load    : in  std_logic;            --! Load on next clock
    par_in  : in  std_logic_vector(1 to LENGTH);  --! Parallel input data
    ser_in  : in  std_logic;            --! Serial input data
    ser_out : out std_logic);           --! Serial output data
end sr_piso;

architecture sr_arch of sr_piso is

  signal my_sr : std_logic_vector(1 to LENGTH);

begin  -- sr_arch
  
  shift_left : if DIR = 1 generate
    ser_out <= my_sr(1) after 2 ns;
  end generate shift_left;

  shift_right : if DIR /= 1 generate
    ser_out <= my_sr(LENGTH) after 2 ns;
  end generate shift_right;

  posedge_left : if (EDGE = 1) and (DIR = 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '1' then   -- Positive clock edge
        if load = '1' then
          my_sr <= par_in;
        else
          my_sr(1 to (LENGTH-1)) <= my_sr(2 to LENGTH);
          my_sr(LENGTH)          <= ser_in;
        end if;
      end if;
    end process shift;
  end generate posedge_left;

  posedge_right : if (EDGE = 1) and (DIR /= 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '1' then   -- Positive clock edge
        if load = '1' then
          my_sr <= par_in;
        else
          my_sr(2 to LENGTH) <= my_sr(1 to LENGTH-1);
          my_sr(1)           <= ser_in;
        end if;
      end if;
    end process shift;
  end generate posedge_right;

  negedge_left : if (EDGE /= 1) and (DIR = 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '0' then   -- Negative clock edge
        if load = '1' then
          my_sr <= par_in;
        else
          my_sr(1 to (LENGTH-1)) <= my_sr(2 to LENGTH);
          my_sr(LENGTH)          <= ser_in;
        end if;
      end if;
    end process shift;
  end generate negedge_left;

  negedge_right : if (EDGE /= 1) and (DIR /= 1) generate
    shift : process(clk)

    begin
      if clk'event and clk = '0' then   -- Negative clock edge
        if load = '1' then
          my_sr <= par_in;
        else
          my_sr(2 to LENGTH) <= my_sr(1 to LENGTH-1);
          my_sr(1)           <= ser_in;
        end if;
      end if;
    end process shift;
  end generate negedge_right;

end sr_arch;




-----------------------------------------------------------------------------
-- Synchronously clocked shift register for spi_interface
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


--! Synchronously clocked parallell input, serial output shift register

--! The shift register length is generic, from 2 and up.
--! The shift direction can be choosen.

entity sr_piso_s is
  generic (
    LENGTH : positive := 8;             --! Shift register length
    DIR    : natural  := 1);            --! Direction; 1=left, 0=right
  port (
    fastclk : in  std_logic;            --! Synchronous clock
    clk_en  : in  std_logic;            --! Clock enable input
    load_en : in  std_logic;            --! Load enable input
    par_in  : in  std_logic_vector(1 to LENGTH);  --! Parallel input data
    ser_in  : in  std_logic;            --! Serial input data
    ser_out : out std_logic);           --! Serial output data
end sr_piso_s;


architecture ar_piso_s_arch of sr_piso_s is

  signal my_sr : std_logic_vector(1 to LENGTH);

begin  -- ar_piso_s_arch

  shift_left_out : if DIR = 1 generate
    ser_out <= my_sr(1) after 2 ns;
  end generate shift_left_out;

  shift_right_out : if DIR /= 1 generate
    ser_out <= my_sr(LENGTH) after 2 ns;
  end generate shift_right_out;

  shift_left : if (DIR = 1) generate
    shift : process(fastclk)

    begin
      if rising_edge(fastclk) then      -- Positive clock edge
        if load_en = '1' then
          my_sr <= par_in;
        elsif clk_en = '1' then
          my_sr(1 to (LENGTH-1)) <= my_sr(2 to LENGTH);
          my_sr(LENGTH)          <= ser_in;
        end if;
      end if;
    end process shift;
  end generate shift_left;

  shift_right : if (DIR /= 1) generate
    shift : process(fastclk)

    begin
      if rising_edge(fastclk) then      -- Positive clock edge
        if load_en = '1' then
          my_sr <= par_in;
        elsif clk_en = '1' then
          my_sr(2 to LENGTH) <= my_sr(1 to LENGTH-1);
          my_sr(1)           <= ser_in;
        end if;
      end if;
    end process shift;
  end generate shift_right;
  

end ar_piso_s_arch;






-----------------------------------------------------------------------------
-- The adc_interface
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity interface_ads1271 is
  
  port (
    fastclk  : in  std_logic;           -- Fast clock
    adclk    : in  std_logic;           -- A/D converter clock
    resync   : in  std_logic;           -- Clock change; resync
    ser_data : in  std_logic;           -- Serial data in
    n_drdy   : in  std_logic;           -- Data is ready, active low
    data_out : out std_logic_vector(23 downto 0);  -- Parallell data out
    nsync    : out std_logic := '1';    -- Synchronize (restart) A/D conv.
    busy     : out std_logic;           -- Busy reading serial data
    g_sclk   : out std_logic);          -- Gated SPI clock, FOR SIMULATION ONLY

end interface_ads1271;


architecture interface_ads1271_arch of interface_ads1271 is

  type   state_type is (idle, counting, freeze);
  signal state         : state_type                    := idle;  -- Controlling state machine
  signal unlatch_data  : std_logic_vector(23 downto 0) := (others => '0');  -- From shift register
  signal bitcounter    : unsigned(4 downto 0)          := to_unsigned(0, 5);  -- Range 0..31
  signal g_sclk_enable : std_logic                     := '0';

  type   rsync_sm_type is (rs_idle, rs_clo1, rs_chi, rs_clo2);
  signal rsync_sm : rsync_sm_type := rs_idle;

  component sr_sipo is
    generic (
      LENGTH : positive;                --! Shift register length
      EDGE   : natural;                 --! Active edge; 1=positive, 0=negative
      DIR    : natural);                --! Direction; 1=left, 0=right
    port (
      clk     : in  std_logic;          --! Clock input
      ser_in  : in  std_logic;          --! Serial input data
      par_out : out std_logic_vector(1 to LENGTH));  --! Parallel output data
  end component;

  
begin  -- interface_ads1271_arch

  sr1 : sr_sipo generic map (
    LENGTH => 24,
    EDGE   => 1,
    DIR    => 1)
    port map (
      clk     => adclk,
      ser_in  => ser_data,
      par_out => unlatch_data);


-- Static interconnects
  g_sclk <= g_sclk_enable and adclk;



-- purpose: Freeze output data 
-- type   : sequential
-- inputs : adclk, n_drdy

  freeze_proc : process (adclk)
  begin  -- process freeze_proc
    if rising_edge(adclk) then          -- rising clock edge
      case state is
        when idle => if n_drdy = '0' then
                       busy          <= '1';
                       state         <= counting;
                       bitcounter    <= to_unsigned(23, 5);
                       g_sclk_enable <= '1';
                     else
                       busy <= '0';
                     end if;
        when counting => busy <= '1';
                         if bitcounter > 0 then
                           bitcounter <= bitcounter - 1;
                         else
                           state         <= freeze;
                           data_out      <= unlatch_data;
                           g_sclk_enable <= '0';
                         end if;
        when freeze => busy <= '0';
                       if n_drdy = '1' then
                         state <= idle;
                       end if;
        when others => state <= idle; busy <= '0';
      end case;
    end if;
  end process freeze_proc;



-- purpose: Handle resyncronization of A/D converters
--          Keep nsync low for at least one adclk cycle
-- type   : sequential
-- inputs : fastclk, adclk, resync
-- outputs: nsync
  resync_proc : process (fastclk)
  begin  -- process resync_proc
    if rising_edge(fastclk) then        -- rising clock edge
      nsync <= '0';
      case rsync_sm is
        when rs_idle => if resync = '1' then
                          rsync_sm <= rs_clo1;
                        else
                          nsync <= '1';
                        end if;
        when rs_clo1 => if adclk = '0' then
                          rsync_sm <= rs_chi;
                        end if;
        when rs_chi => if adclk = '1' then
                         rsync_sm <= rs_clo2;
                       end if;
    when rs_clo2 => if adclk = '0' then
                      rsync_sm <= rs_idle;
                    end if;
    when others => rsync_sm <= rs_idle;
    nsync                   <= '1';
  end case;
end if;
end process resync_proc;

end interface_ads1271_arch;




-----------------------------------------------------------------------------
-- The priority_resolver2
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity priority_resolver2 is
  port (
    inp    : in  std_logic_vector(11 downto 0);  -- Active low inputs
    prio   : out unsigned(3 downto 0);  -- Output highest signal
    active : out std_logic);            -- High when any input is active
end priority_resolver2;


architecture priority_resolver2_arch of priority_resolver2 is

begin  -- priority_resolver2_arch

  -- Any input active ???
  active <= inp(0) or inp(1) or inp(2) or inp(3) or inp(4) or inp(5) or
            inp(6) or inp(7) or inp(8) or inp(9) or inp(10) or inp(11) after 1 ns;

  -- purpose: Priority resolver
  -- type   : combinational
  -- inputs : inp
  -- outputs: prio, active
  mainloop : process (inp)
  begin  -- process mainloop
    if inp(11) = '1' then
      prio <= to_unsigned(11, 4) after 1 ns;
    elsif inp(10) = '1' then
      prio <= to_unsigned(10, 4) after 1 ns;
    elsif inp(9) = '1' then
      prio <= to_unsigned(9, 4) after 1 ns;
    elsif inp(8) = '1' then
      prio <= to_unsigned(8, 4) after 1 ns;
    elsif inp(7) = '1' then
      prio <= to_unsigned(7, 4) after 1 ns;
    elsif inp(6) = '1' then
      prio <= to_unsigned(6, 4) after 1 ns;
    elsif inp(5) = '1' then
      prio <= to_unsigned(5, 4) after 1 ns;
    elsif inp(4) = '1' then
      prio <= to_unsigned(4, 4) after 1 ns;
    elsif inp(3) = '1' then
      prio <= to_unsigned(3, 4) after 1 ns;
    elsif inp(2) = '1' then
      prio <= to_unsigned(2, 4) after 1 ns;
    elsif inp(1) = '1' then
      prio <= to_unsigned(1, 4) after 1 ns;
    else
      prio <= to_unsigned(0, 4) after 1 ns;
    end if;
  end process mainloop;
  
end priority_resolver2_arch;




-----------------------------------------------------------------------------
-- The active_input state machine
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity active_input is
  port (
    fastclk    : in  std_logic;         -- Main clock
    enable     : in  std_logic;         -- Enable signal
    busy_n     : in  std_logic;         -- Busy input signal, active low
    clear      : in  std_logic;         -- Clear the corresponding flip-flop
    active_out : out std_logic);        -- Active output
end active_input;


architecture active_input_arch of active_input is

  type   input_state_type is (i_idle, i_active, i_decay);
  signal i_state         : input_state_type := i_idle;
  signal temp_active_out : std_logic        := '0';  -- Temporary signal'


begin  -- active_input_arch

  -- Static delayed connex
  active_out <= temp_active_out after 1 ns;


  -- purpose: Input state machine
  -- type   : sequential
  -- inputs : fastclk, fastclk, busy_n, clear
  -- outputs: temp_active_out
  input_active : process (fastclk)

  begin  -- process input_active
    if rising_edge(fastclk) then        -- rising clock edge
      temp_active_out <= '0';
      case i_state is
        when i_idle => if (busy_n = '0') and (enable = '1') then
                         temp_active_out <= '1';
                         i_state         <= i_active;
                       end if;

        when i_active => if clear = '0' then
                           temp_active_out <= '1';
                         else
                           i_state <= i_decay;
                         end if;

        when i_decay => if busy_n = '1' then
                          i_state <= i_idle;
                        end if;
                        
        when others => i_state <= i_idle;
      end case;

    end if;
  end process input_active;
end active_input_arch;






-----------------------------------------------------------------------------
-- The one_of_n_encoder
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity one_of_n_encoder is
  
  port (
    number     : in  unsigned(3 downto 0);
    enable     : in  std_logic;
    single_out : out std_logic_vector(11 downto 0));  -- Only a single wire is active at a time

end one_of_n_encoder;



architecture one_of_n_encoder_arch of one_of_n_encoder is

  type     out_table_type is array (0 to 15) of std_logic_vector(11 downto 0);
  constant out_table : out_table_type :=
    ("000000000001",
     "000000000010",
     "000000000100",
     "000000001000",
     "000000010000",
     "000000100000",
     "000001000000",
     "000010000000",
     "000100000000",
     "001000000000",
     "010000000000",
     "100000000000",
     "000000000000",
     "000000000000",
     "000000000000",
     "000000000000");

begin  -- one_of_n_encoder_arch

  -- purpose: Translate unsigned number into a signle output
  -- type   : sequential
  -- inputs : number, number, enable
  -- outputs: single_out
  xlate : process (number, enable)
  begin  -- process xlate
    if enable = '1' then
      single_out <= out_table(to_integer(number)) after 1 ns;
    else
      single_out <= "000000000000" after 1 ns;
    end if;
    
  end process xlate;

end one_of_n_encoder_arch;








-----------------------------------------------------------------------------
-- The master_state_machine
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.polyamplib.all;

entity master_state_machine is
  
  port (

    fastclk    : in    std_logic := '0';               -- Main clock
    enable_lo8 : in    std_logic;       -- Enable the 8 low inputs
    busy_n_vec : in    std_logic_vector(11 downto 0);  -- Busy inputs
    id_code    : inout unsigned(3 downto 0);           -- Id code output
    fifo_write : out   std_logic);      -- Save to fifo

end master_state_machine;



architecture master_state_machine_arch of master_state_machine is

  signal active_data   : std_logic_vector(11 downto 0);
  signal clear_vec     : std_logic_vector(11 downto 0);
  signal any_active    : std_logic := '0';  -- Is any input active ???
  signal clear_enable  : std_logic := '0';  -- Clear the current active input flip-flop
  signal temp_clear_en : std_logic := '0';
  signal priority      : unsigned(3 downto 0);

  type   master_state_type is (m_idle, m_latch, m_clear, m_decay);
  signal m_state : master_state_type := m_idle;
  

  
begin  -- master_state_machine_arch

  -- The first 8 are controlled by the enable_lo8 signal
  gen1 : for i in 0 to 7 generate
    inp_sm : active_input port map (
      fastclk    => fastclk,
      enable     => enable_lo8,
      busy_n     => busy_n_vec(i),
      clear      => clear_vec(i),
      active_out => active_data(i));
  end generate gen1;

  -- The other inputs are always enabled
  gen2 : for i in 8 to 11 generate
    inp_sm : active_input port map (
      fastclk    => fastclk,
      enable     => '1',
      busy_n     => busy_n_vec(i),
      clear      => clear_vec(i),
      active_out => active_data(i));
  end generate gen2;

  resolve : priority_resolver2 port map (
    inp    => active_data,
    prio   => priority,
    active => any_active);

  encode : one_of_n_encoder port map (
    number     => id_code,
    enable     => clear_enable,
    single_out => clear_vec);


  clear_enable <= temp_clear_en after 1 ns;  -- After some delay
  fifo_write   <= clear_enable;              -- Same-same


  -- purpose: Master state machine
  -- type   : sequential
  -- inputs : fastclk, s0..s11,c0..c11
  -- outputs: fifo_write, data_id
  master_loop : process (fastclk)

  begin  -- process master_loop

    if rising_edge(fastclk) then        -- rising clock edge
      temp_clear_en <= '0';
      case m_state is
        when m_idle => if any_active = '1' then
                         id_code <= priority after 1 ns;
                         m_state <= m_latch;
                       end if;

        when m_latch =>
          temp_clear_en <= '1';
          m_state       <= m_clear;

        when m_clear =>
          m_state <= m_decay;

        when m_decay =>
          if any_active = '1' then
            id_code <= priority after 1 ns;
            m_state <= m_latch;
          else
            m_state <= m_idle;
          end if;

        when others => m_state <= m_idle;
      end case;
    end if;

  end process master_loop;
  
end master_state_machine_arch;





-----------------------------------------------------------------------------
-- The fifo_memory
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use work.polyamplib.all;

entity fifo_memory is
  generic (
    fifo_size : natural := 100);
  port (
    clock   : in  std_logic;
    sclr    : in  std_logic;
    datain  : in  std_logic_vector(31 downto 0);
    wrreq   : in  std_logic;
    rdreq   : in  std_logic;
    dataout : out std_logic_vector(31 downto 0);
    full    : out std_logic;
    empty   : out std_logic;
    meter   : out unsigned(7 downto 0));
end fifo_memory;



architecture fifo_memory_arch of fifo_memory is

begin  -- fifo_memory_arch

  -- purpose: Fifo memory simulator
  -- type   : sequential
  -- inputs : clock, datain, wrreq, rdreq, clock
  -- outputs: dataout, full, empty
  fifo_mem : process (clock)

    subtype  word is std_logic_vector(31 downto 0);
    type     memory_area is array(0 to fifo_size) of word;
    variable fifo : memory_area;        -- The fifo memory area

    variable head_ix : natural := 0;    -- Write data to this address
    variable tail_ix : natural := 0;    -- Read data from this address
    variable test_ix : natural := 0;    -- Used for test of overflow/underflow
    
  begin  -- process fifo
    if rising_edge(clock) then          -- rising clock edge

      -- Synchronous clear
      if sclr = '1' then
        head_ix := 0;
        tail_ix := 0;
      elsif wrreq = '1' then            -- Fifo write op's
        test_ix := head_ix + 1;
        if test_ix > fifo_size then
          test_ix := 0;
        end if;
        -- Writing to full fifo discards the last value
        if test_ix /= tail_ix then      -- NOT full
          fifo(head_ix) := datain;      -- Write data
          head_ix       := test_ix;     -- And adjust the pointer
        end if;
      end if;

      -- Reading empty fifo returns the last value
      if (rdreq = '1') and (head_ix /= tail_ix) then
        dataout <= fifo(tail_ix) after 1 ns;
        tail_ix := tail_ix + 1;
        if tail_ix > fifo_size then
          tail_ix := 0;
        end if;
      end if;

      -- Fifo empty signal
      if head_ix = tail_ix then
        empty <= '1' after 1 ns;
      else
        empty <= '0' after 1 ns;
      end if;

      -- Fifo full signal
      if (tail_ix = (head_ix+1)) or ((tail_ix = 0) and (head_ix = fifo_size)) then
        full <= '1' after 1 ns;
      else
        full <= '0' after 1 ns;
      end if;

      -- Fifo fill meter operations
      if head_ix >= tail_ix then
        meter <= to_unsigned(head_ix - tail_ix, 8);
      else
        meter <= to_unsigned(fifo_size + 1 + head_ix - tail_ix, 8);
      end if;
      
    end if;
  end process fifo_mem;

end fifo_memory_arch;






-----------------------------------------------------------------------------
-- The fifo_ft_memory
-----------------------------------------------------------------------------

-- Fallthrough fifo memory model


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--use work.polyamplib.all;

entity fifo_ft_memory is
  generic (
    fifo_size : natural := 100);
  port (
    clock   : in  std_logic;
    sclr    : in  std_logic;
    datain  : in  std_logic_vector(31 downto 0);
    wrreq   : in  std_logic;
    rdreq   : in  std_logic;
    dataout : out std_logic_vector(31 downto 0);
    full    : out std_logic;
    empty   : out std_logic;
    meter   : out unsigned(7 downto 0));
end fifo_ft_memory;



architecture fifo_ft_memory_arch of fifo_ft_memory is

begin  -- fifo_ft_memory_arch

  -- purpose: Fifo memory simulator
  -- type   : sequential
  -- inputs : clock, datain, wrreq, rdreq, clock
  -- outputs: dataout, full, empty
  fifo_mem : process (clock)

    subtype  word is std_logic_vector(31 downto 0);
    type     memory_area is array(0 to fifo_size) of word;
    variable fifo : memory_area;        -- The fifo memory area

    variable head_ix : natural := 0;    -- Write data to this address
    variable tail_ix : natural := 0;    -- Read data from this address
    variable test_ix : natural := 0;    -- Used for test of overflow/underflow

  begin  -- process fifo
    if rising_edge(clock) then          -- rising clock edge

      -- Synchronous clear
      if sclr = '1' then
        head_ix := 0;
        tail_ix := 0;
      else
        -- Write to fifo
        if wrreq = '1' then             -- Fifo write op's
          test_ix := head_ix + 1;
          if test_ix > fifo_size then
            test_ix := 0;
          end if;

          -- Fifo is empty
          if head_ix = tail_ix then
            dataout       <= datain;     -- Let data fall-through
            fifo(head_ix) := datain;     -- Write data
            head_ix       := test_ix;    -- And adjust the pointer
          elsif test_ix /= tail_ix then  -- NOT full
            fifo(head_ix) := datain;     -- Write data
            head_ix       := test_ix;    -- And adjust the pointer
          end if;
        end if;

        -- Reading empty fifo returns the last value
        if (rdreq = '1') and (head_ix /= tail_ix) then
          tail_ix := tail_ix + 1;
          if tail_ix > fifo_size then
            tail_ix := 0;
          end if;
          if tail_ix /= head_ix then
            dataout <= fifo(tail_ix) after 1 ns;
          end if;
        end if;
      end if;


      -- Fifo empty signal
      if head_ix = tail_ix then
        empty <= '1' after 1 ns;
      else
        empty <= '0' after 1 ns;
      end if;

      -- Fifo full signal
      if (tail_ix = (head_ix+1)) or ((tail_ix = 0) and (head_ix = fifo_size)) then
        full <= '1' after 1 ns;
      else
        full <= '0' after 1 ns;
      end if;

      -- Fifo fill meter operations
      if head_ix >= tail_ix then
        meter <= to_unsigned(head_ix - tail_ix, 8);
      else
        meter <= to_unsigned(fifo_size + 1 + head_ix - tail_ix, 8);
      end if;
      
    end if;
  end process fifo_mem;

end fifo_ft_memory_arch;






-----------------------------------------------------------------------------
-- VHDL model of A/D-converter Texas ADS1271
-----------------------------------------------------------------------------

-- NOTE! This is a model for simulation only. It is not
--       coded to be synthezised.

-- SPI interface format, 512 clock cycles/conversion

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity ads1271_model is
  port (
    analog_in : in  signed(23 downto 0);  -- Analog input signal
    clk       : in  std_logic;            -- Conversion clock
    sclk      : in  std_logic;            -- SPI clock
    n_sync    : in  std_logic;            -- Input sync signal, active low
    din       : in  std_logic;            -- Serial data in
    dout      : out std_logic;            -- Serial data out
    n_drdy    : out std_logic := '1');    -- Data ready, active low
end ads1271_model;



architecture ads1271_model_arch of ads1271_model is

  signal analog_data            : signed(23 downto 0) := to_signed(0, 24);
  shared variable conv_timer    : integer             := 2;  -- Data conversion rate
  shared variable sdata_counter : integer             := -1;  -- Controls dout bits
  signal din_mem                : std_logic           := '0';  -- 'din' memory
  signal drdy_ff                : std_logic;
  signal clk_del                : std_logic           := '0';  -- Delayed clk signal
  signal sclk_del               : std_logic           := '0';  -- Delayed sclk signal
  signal ct_is_one              : std_logic;  -- Locally generated sync
  signal ct_is_zero             : std_logic;  -- Locally generated sync

  
begin  -- ads1271_model_arch

  n_drdy   <= drdy_ff or ct_is_one after 8 ns;
  sclk_del <= sclk                 after 2 ns;
  clk_del  <= clk                  after 1 ns;


  -- purpose: Data conversion circuitry
  -- type   : sequential
  -- inputs : clk_del
  -- outputs: dout, ct_is_one
  convert : process (clk_del)
  begin  -- process convert
    if falling_edge(clk_del) then       -- falling clock edge
      ct_is_one  <= '0';
      ct_is_zero <= '0';
      if n_sync = '0' then
        conv_timer := (128*512);        -- Reloading the FIR takes a while
        ct_is_one  <= '1';
      else
        conv_timer    := conv_timer - 1;
        if conv_timer <= 0 then         -- Conversion rate number
          conv_timer    := 512;
          sdata_counter := 23;          -- MSBit comes first
          ct_is_zero    <= '1';
        end if;  -- conv_timer <= 0
        if conv_timer = 1 then
          ct_is_one <= '1';
        end if;  -- conv_timer = 1
        if conv_timer = 512 then
          analog_data <= analog_in;  -- Data sampling, data av. after 512 clock's
        end if;  -- conv_timer = 512
      end if;  -- n_sync = '0'
    end if;  -- falling_edge(clk_del)

  end process convert;



  -- purpose: Serial data interface control
  -- type   : sequential
  -- inputs : sclk_del
  -- outputs: dout
  dataout : process (sclk_del)

  begin  -- process dataout
    if sclk_del'event then
      if sclk_del = '0' then            -- falling clock edge
        if sdata_counter >= 0 then
          dout          <= 'X'                        after 5 ns;
          dout          <= analog_data(sdata_counter) after 12 ns;
          sdata_counter := sdata_counter - 1;
        else
          dout <= 'X'     after 5 ns;
          dout <= din_mem after 12 ns;
        end if;
      else
        -- rising clock edge samples din
        din_mem <= din;
      end if;
    end if;
  end process dataout;



  -- purpose: Controls the drdy_ff signal
  -- type   : sequential
  -- inputs : sclk_del, ct_is_zero
  -- outputs: drdy_ff
  readyctl : process (sclk_del, ct_is_zero)
  begin  -- process readyctl
    if ct_is_zero = '1' then            -- asynchronous reset (active high)
      drdy_ff <= '0';
    elsif falling_edge(sclk_del) then   -- falling clock edge
      drdy_ff <= '1';
    end if;
  end process readyctl;

end ads1271_model_arch;





-----------------------------------------------------------------------------
-- SPI slave with secondary spi master port 
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.polyamplib.all;


entity spi_slave is
  
  port (
    -- Main controls
    fastclk : in  std_logic;
    spi_tx  : in  std_logic_vector(31 downto 0);  -- Data to be sent
    spi_rx  : out std_logic_vector(31 downto 0);  -- Data to be received
    spi_op  : out std_logic;            -- Read/Write status/data/command

    -- Slave port, connected to CPU
    mosi    : in  std_logic;
    miso    : out std_logic;
    sck     : in  std_logic;            -- SPI clock
    en_adc  : in  std_logic;            -- Active low, enable ADC
    en_incl : in  std_logic;            -- Active low, enable inclinometer

    -- Master port, connected to inclinometer
    incl_miso : in  std_logic;
    incl_mosi : out std_logic;
    incl_sck  : out std_logic;
    incl_ena  : out std_logic);         -- Active low, enable inclinometer
end spi_slave;



architecture spi_slave_arch of spi_slave is

  type   state_type is (idle, load, shift, relax);
  signal state : state_type := idle;    -- Controlling state machine

  signal enable_inclinometer : std_logic := '0';
  signal enable_adc          : std_logic := '0';
  signal adc_miso            : std_logic;
  signal adc_load            : std_logic := '0';
--  signal g_miso              : std_logic register := '0';  -- guarded miso signal
  signal adc_sck             : std_logic;
  signal tx_sck              : std_logic;
  signal delayed_adc_sck     : std_logic;  -- Delayed, for edge detection
  

begin  -- spi_slave_arch

  -- Transmit shift register
  tx_sr : sr_piso_s generic map (
    LENGTH => 32,
    DIR    => 1)                        -- Shift left => MSB first
    port map (
      fastclk => fastclk,
      clk_en  => tx_sck,
      load_en => adc_load,
      par_in  => spi_tx,
      ser_in  => '0',
      ser_out => adc_miso);

  -- Receive shift register
  rx_sr : sr_sipo generic map (
    LENGTH => 32,
    EDGE   => 1,                        -- Positive edge
    DIR    => 1)                        -- Shift left => MSB first
    port map (
      clk     => adc_sck,
      ser_in  => mosi,
      par_out => spi_rx);


  -- Passthru signals
  incl_sck  <= sck                     after 1 ns;
  incl_mosi <= mosi                    after 1 ns;
  incl_ena  <= not enable_inclinometer after 1 ns;  -- Active low output

  -- Enable and clock
  enable_inclinometer <= not en_incl and en_adc            after 1 ns;
  enable_adc          <= not en_adc                        after 1 ns;
  adc_sck             <= sck or en_adc                     after 1 ns;  -- Clock is driven high at inactive state
  tx_sck              <= (not adc_sck) and delayed_adc_sck after 1 ns;  -- sck negative edge trigger

  -- Output data
--  miso <= g_miso;


  -- purpose: ADC controls miso
--  talkto_adc : block (enable_adc = '1')
--  begin  -- block talkto_adc
--    g_miso <= guarded adc_miso;
--  end block talkto_adc;


  -- purpose: Inclinometer controls miso
--  talkto_incl : block (enable_inclinometer = '1')
--  begin  -- block talkto_incl
--    g_miso <= guarded incl_miso;
--  end block talkto_incl;


  -- Instead of the guarded assignments above
  miso <= adc_miso when enable_inclinometer = '0' else incl_miso;


  -- purpose: SPI slave controller state machine
  -- type   : sequential
  -- inputs : fastclk, enable_adc
  -- outputs: adc_load
  input_active : process (fastclk)

  begin  -- process input_active
    if rising_edge(fastclk) then        -- rising clock edge
      delayed_adc_sck <= adc_sck;
      adc_load        <= '1' after 1 ns;
      spi_op          <= '0' after 1 ns;
      case state is
        when idle => if enable_adc = '1' then
                       state <= load;
                     end if;

        when load => state <= shift;

        when shift => adc_load <= '0' after 1 ns;
                      if enable_adc = '0' then
                        state  <= relax;
                        spi_op <= '1' after 1 ns;
                      end if;

        when relax => state <= idle;

        when others => state <= idle;
      end case;
    end if;
  end process input_active;
  
end spi_slave_arch;




-----------------------------------------------------------------------------
-- SPI slave with burst capabilities and with secondary spi master port 
-- NOTE! Spi mode 2
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.polyamplib.all;


entity spi_slave_burst is
  
  port (
    -- Main controls
    fastclk   : in  std_logic;
    spi_tx    : in  std_logic_vector(31 downto 0);  -- Data to be sent
    spi_rx    : out std_logic_vector(31 downto 0);  -- Data to be received
    exec_cmd  : out std_logic;                      -- Write command/data
    fifo_read : out std_logic;                      -- Read status/data

    -- Slave port, connected to CPU
    mosi    : in  std_logic;
    miso    : out std_logic;
    sck     : in  std_logic;            -- SPI clock
    en_adc  : in  std_logic;            -- Active low, enable ADC
    en_incl : in  std_logic;            -- Active low, enable inclinometer

    -- Master port, connected to inclinometer
    incl_miso : in  std_logic;
    incl_mosi : out std_logic;
    incl_sck  : out std_logic;
    incl_ena  : out std_logic);         -- Active low, enable inclinometer
end spi_slave_burst;



architecture spi_slave_burst_arch of spi_slave_burst is

--  type   state_type is (idle, load, shift, relax);
--  signal state : state_type := idle;    -- Controlling state machine

  signal bitcounter          : unsigned(4 downto 0) := to_unsigned(0, 5);  -- Range 0..31
  signal enable_inclinometer : std_logic            := '0';
  signal enable_adc          : std_logic            := '0';
  signal adc_miso            : std_logic;
  signal clk_tx_sr           : std_logic            := '0';
  signal load_tx_sr          : std_logic            := '0';

  signal sck_r1             : std_logic;  -- Delayed, for edge detection
  signal sck_r2             : std_logic;  -- Delayed, for edge detection
  signal sck_negedge        : std_logic;  -- Negative edge
  signal sck_posedge        : std_logic;  -- Positive edge
  signal enable_adc_r1      : std_logic;  -- Delayed, for edge detection
  signal enable_adc_r2      : std_logic;  -- Delayed, for edge detection
  signal enable_adc_negedge : std_logic;  -- Negative edge

  signal bitcounter_zero : std_logic;
  signal bitcounter_one  : std_logic;
  signal exec_ff         : std_logic := '0';  -- Enable the execute signal
  signal load_ff         : std_logic := '0';  -- Enable the load signal
  
  
begin  -- spi_slave_burst_arch

  -- Transmit shift register
  tx_sr : sr_piso_s generic map (
    LENGTH => 32,
    DIR    => 1)                        -- Shift left => MSB first
    port map (
      fastclk => fastclk,
      clk_en  => clk_tx_sr,
      load_en => load_tx_sr,
      par_in  => spi_tx,
      ser_in  => '0',
      ser_out => adc_miso);

  -- Receive shift register
  rx_sr : sr_sipo generic map (
    LENGTH => 32,
    EDGE   => 1,                        -- Positive edge
    DIR    => 1)                        -- Shift left => MSB first
    port map (
      clk     => sck_negedge,
      ser_in  => mosi,
      par_out => spi_rx);


  -- Passthru signals
  incl_sck  <= sck                     after 1 ns;
  incl_mosi <= mosi                    after 1 ns;
  incl_ena  <= not enable_inclinometer after 1 ns;  -- Active low output

  -- Enable and clock
  enable_inclinometer <= not en_incl and en_adc after 1 ns;
  enable_adc          <= not en_adc             after 1 ns;

  sck_posedge <= sck_r1 and (not sck_r2) after 1 ns;  -- sck Positive edge
  sck_negedge <= sck_r2 and (not sck_r1) after 1 ns;  -- sck Negative edge

  enable_adc_negedge <= enable_adc_r1 and (not enable_adc_r2) after 1 ns;
  fifo_read          <= sck_posedge and bitcounter_one        after 1 ns;
  load_tx_sr         <= enable_adc_negedge or
                        (sck_negedge and bitcounter_zero and (not load_ff)) after 1 ns;
  exec_cmd  <= bitcounter_zero and exec_ff and sck_posedge after 1 ns;
  clk_tx_sr <= sck_posedge or enable_adc_negedge           after 1 ns;

  -- Instead of the guarded assignments above
  miso <= adc_miso when enable_inclinometer = '0' else incl_miso after 1 ns;


  -- purpose: SPI slave controller state machine
  -- type   : sequential
  -- inputs : fastclk, sck, enable_adc, bitcounter, load_tx_sr
  -- outputs: sck_r1, sck_r2, enable_adc_r1, enable_adc_r2, bitcounter
  input_active : process (fastclk)

  begin  -- process input_active
    if rising_edge(fastclk) then        -- rising clock edge
      sck_r2 <= sck_r1 after 1 ns;
      sck_r1 <= sck    after 1 ns;

      enable_adc_r2 <= enable_adc_r1 after 1 ns;
      enable_adc_r1 <= enable_adc    after 1 ns;

      if enable_adc_negedge = '1' then
        bitcounter <= to_unsigned(0, 5) after 1 ns;
      else if sck_negedge = '1' then
             bitcounter <= bitcounter + 1 after 1 ns;
           end if;
      end if;

      if bitcounter = 0 then
        bitcounter_zero <= '1' after 1 ns;
      else
        bitcounter_zero <= '0' after 1 ns;
      end if;

      if bitcounter = 1 then
        bitcounter_one <= '1' after 1 ns;
      else
        bitcounter_one <= '0' after 1 ns;
      end if;

      if enable_adc = '0' then
        exec_ff <= '0' after 1 ns;
      elsif bitcounter = 2 then
        exec_ff <= '1' after 1 ns;
      end if;

      if load_tx_sr = '1' then
        load_ff <= '1' after 1 ns;
      elsif bitcounter = 2 then
        load_ff <= '0' after 1 ns;
      end if;

    end if;
  end process input_active;

end spi_slave_burst_arch;





----------------------------------------------------------------------
-- Command decoder function
----------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity command_decoder is
  port (
    addr_data  : in  std_logic_vector(31 downto 0);  -- Input address/data
    decode     : in  std_logic;         -- Single cycle decode pulse
    fastclk    : in  std_logic;         -- Master clock (not used for now)
    sel_nulcmd : out std_logic;         -- NULL command (no operation)
    sel_adclk0 : out std_logic;         -- Select sampling clock, ad0.
    sel_adclk1 : out std_logic;         -- Select sampling clock, ad1.
    sel_adclk2 : out std_logic;         -- Select sampling clock, ad2.
    sel_adclk3 : out std_logic;         -- Select sampling clock, ad3.
    sel_adclk4 : out std_logic;         -- Select sampling clock, ad4.
    sel_adclk5 : out std_logic;         -- Select sampling clock, ad5.
    sel_adclk6 : out std_logic;         -- Select sampling clock, ad6.
    sel_adclk7 : out std_logic;         -- Select sampling clock, ad7.
    resync_adc : out std_logic;         -- Resynchronize all ADC's
    write_ctrl : out std_logic;         -- Write to control-signal register
    start_adcs : out std_logic;         -- Start AD-conversion
    stop_adcs  : out std_logic);        -- Stop AD-conversion
end command_decoder;


architecture command_decoder_arch of command_decoder is

begin  -- command_decoder_arch

  sel_nulcmd <= '1' when (addr_data(27 downto 24) = "0000") and decode = '1' else '0';
  sel_adclk0 <= '1' when (addr_data(27 downto 24) = "0001") and decode = '1' else '0';
  sel_adclk1 <= '1' when (addr_data(27 downto 24) = "0010") and decode = '1' else '0';
  sel_adclk2 <= '1' when (addr_data(27 downto 24) = "0011") and decode = '1' else '0';
  sel_adclk3 <= '1' when (addr_data(27 downto 24) = "0100") and decode = '1' else '0';
  sel_adclk4 <= '1' when (addr_data(27 downto 24) = "0101") and decode = '1' else '0';
  sel_adclk5 <= '1' when (addr_data(27 downto 24) = "0110") and decode = '1' else '0';
  sel_adclk6 <= '1' when (addr_data(27 downto 24) = "0111") and decode = '1' else '0';
  sel_adclk7 <= '1' when (addr_data(27 downto 24) = "1000") and decode = '1' else '0';
  resync_adc <= '1' when (addr_data(27 downto 24) = "1001") and decode = '1' else '0';
  write_ctrl <= '1' when (addr_data(27 downto 24) = "1010") and decode = '1' else '0';
  start_adcs <= '1' when (addr_data(27 downto 24) = "1011") and decode = '1' else '0';
  stop_adcs  <= '1' when (addr_data(27 downto 24) = "1100") and decode = '1' else '0';

end command_decoder_arch;




----------------------------------------------------------------------
-- ADC clock multiplexer function
----------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity clockmux is
  port (
    clk24M  : in  std_logic;             -- Input clocks, 24 576 000
    clk4M   : in  std_logic;             -- 4 096 000
    clk2M   : in  std_logic;             -- 2 048 000
    clk1M   : in  std_logic;             -- 1 024 000
    clk512k : in  std_logic;             -- 512 000
    clk256k : in  std_logic;             -- 256 000
    clk128k : in  std_logic;             -- 128 000
    sel     : in  unsigned(2 downto 0);  -- Mux select input
    clkout  : out std_logic);            -- Output clock
end clockmux;


architecture clockmux_arch of clockmux is

begin  -- clockmux_arch

  with sel select
    clkout <=
    clk128k when "000",
    clk256k when "001",
    clk512k when "010",
    clk1M   when "011",
    clk2M   when "100",
    clk4M   when "101",
    clk24M  when "110",
    clk24M  when others;

end clockmux_arch;




-------------------------------------------------------------------------------
-- SPI master for simulation and test (no synth.)
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity spi_master is
  port (
    -- Hardware ports
    miso     : in    std_logic;
    mosi     : out   std_logic;
    sck      : inout std_logic := '0';
    en_adval : out   std_logic := '1';
    en_incl  : out   std_logic := '1';

    -- Simulation ports
    data_to_spi   : in  std_logic_vector(31 downto 0);
    data_from_spi : out std_logic_vector(31 downto 0);
    start         : in  std_logic;
    busy          : out std_logic := '0';
    running       : in  std_logic);
end spi_master;


architecture spi_master_arch of spi_master is

  type   state_type is (idle, load, shift);
  signal state     : state_type := idle;
  signal start_mem : std_logic  := '0';
  
  
begin  -- spi_master_arch


  -----------------------------------------------------------------------------

  -- Assume that data_to_spi is defined before this process is triggered
  spi_rxtx : process (sck)

    variable bit_index : integer := 0;
    
  begin  -- process spi_rxtx
    if sck'event then
      if sck = '1' then                 -- rising clock edge
        start_mem <= start;
        case state is
          when idle => if start = '1' and start_mem = '0' then  -- Start rising
                                                                -- edge
                         en_adval  <= '0' after 15 ns;
                         busy      <= '1' after 2 ns;
                         state     <= load;
                         bit_index := 31;
                       end if;

          when load => data_from_spi(bit_index) <= miso;
                       state <= shift;

          when shift => bit_index := bit_index - 1;
                        if bit_index < 0 then
                          state    <= idle;
                          en_adval <= '1' after 15 ns;
                          busy     <= '0' after 2 ns;
                        else
                          data_from_spi(bit_index) <= miso;
                        end if;
          when others => state <= idle;
        end case;
      end if;  -- sck = '1' else ...

      if sck = '0' and (state = load or state = shift) then
        mosi <= data_to_spi(bit_index) after 1 ns;
      end if;

    end if;  -- sck'event
  end process spi_rxtx;

  -----------------------------------------------------------------------------

  spi_clk : process
  begin  -- process spi_clk
    while running = '1' loop
      sck <= not sck;
      wait for 50 ns;
    end loop;
    sck <= not sck;
    wait for 50 ns;
    sck <= not sck;
    wait for 50 ns;
    sck <= not sck;
    wait for 50 ns;
    sck <= not sck;
    wait for 50 ns;
    wait;
  end process spi_clk;

end spi_master_arch;



----------------------------------------------------------------------
-- Synchronization logic
----------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sync_logic_2 is
  
  port (
    start_adcs       : in  std_logic;   -- Start command
    stop_adcs        : in  std_logic;   -- Stop command
    reset            : in  std_logic;   -- Active high
    hwsync           : in  std_logic;   -- Hardware sync
    fastclk          : in  std_logic;   -- Master clock
    enable_adcvalues : out std_logic);  -- Enable reception of values

end sync_logic_2;



architecture sync_logic_2_arch of sync_logic_2 is

  signal del_sync_1 : std_logic;         -- 1'st delay
  signal del_sync_2 : std_logic;         -- 2'nd delay
  signal sr_set     : std_logic;         -- Set input to SR-latch
  signal sr         : std_logic := '0';  -- sr Q output, entity output

  
begin  -- sync_logic_2_arch

  enable_adcvalues <= sr;
  sr_set           <= (del_sync_1 and (not del_sync_2)) xor start_adcs;


  -- purpose: Two D-latches and one SR-latch is controlled
  -- type   : sequential
  -- inputs : fastclk, reset, start_adcs, stop_adcs, hwsync
  -- outputs: sr
  registered_logic : process (fastclk, reset)
  begin  -- process registered_logic
    if reset = '1' then                 -- asynchronous reset
      del_sync_1 <= '0' after 2 ns;
      del_sync_2 <= '0' after 2 ns;
      sr         <= '0' after 2 ns;
    elsif rising_edge(fastclk) then     -- rising clock edge
      del_sync_1 <= hwsync     after 2 ns;
      del_sync_2 <= del_sync_1 after 2 ns;
      if stop_adcs = '1' then
        sr <= '0' after 2 ns;
      elsif sr_set = '1' then
        sr <= '1' after 2 ns;
      end if;

    end if;
  end process registered_logic;

end sync_logic_2_arch;

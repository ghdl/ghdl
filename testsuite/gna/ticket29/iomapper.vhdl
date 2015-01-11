use WORK.ALL;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use work.debugtools.all;

entity iomapper is
  port (Clk : in std_logic;
        cpuclock : in std_logic;
        pixelclk : in std_logic;
        uartclock : in std_logic;
        clock50mhz : in std_logic;
        phi0 : in std_logic;
        reset : in std_logic;
        reset_out : out std_logic;
        irq : out std_logic;
        nmi : out std_logic;
        restore_nmi : out std_logic;
        cpu_hypervisor_mode : in std_logic;
        address : in std_logic_vector(19 downto 0);
        r : in std_logic;
        w : in std_logic;
        data_i : in std_logic_vector(7 downto 0);
        data_o : out std_logic_vector(7 downto 0);
        sd_data_o : out std_logic_vector(7 downto 0);
        sector_buffer_mapped : out std_logic;

        key_scancode : in unsigned(15 downto 0);
        key_scancode_toggle : in std_logic;
        
        reg_isr_out : out unsigned(7 downto 0);
        imask_ta_out : out std_logic;
        
        led : out std_logic := '0';
        motor : out std_logic := '0';

        ps2data : in std_logic;
        ps2clock : in std_logic;

        pixel_stream_in : in unsigned (7 downto 0);
        pixel_y : in unsigned (11 downto 0);
        pixel_valid : in std_logic;
        pixel_newframe : in std_logic;
        pixel_newraster : in std_logic;
    
    ---------------------------------------------------------------------------
    -- IO lines to the ethernet controller
    ---------------------------------------------------------------------------
    eth_mdio : inout std_logic;
    eth_mdc : out std_logic;
    eth_reset : out std_logic;
    eth_rxd : in unsigned(1 downto 0);
    eth_txd : out unsigned(1 downto 0);
    eth_txen : out std_logic;
    eth_rxdv : in std_logic;
    eth_rxer : in std_logic;
    eth_interrupt : in std_logic;

        ----------------------------------------------------------------------
        -- Flash RAM for holding config
        ----------------------------------------------------------------------
        QspiSCK : out std_logic;
        QspiDB : inout std_logic_vector(3 downto 0);
        QspiCSn : out std_logic;        

        -------------------------------------------------------------------------
        -- Lines for the SDcard interface itself
        -------------------------------------------------------------------------
        cs_bo : out std_logic;
        sclk_o : out std_logic;
        mosi_o : out std_logic;
        miso_i : in  std_logic;

        ---------------------------------------------------------------------------
        -- Lines for other devices that we handle here
        ---------------------------------------------------------------------------
        aclMISO : in std_logic;
        aclMOSI : out std_logic;
        aclSS : out std_logic;
        aclSCK : out std_logic;
        aclInt1 : in std_logic;
        aclInt2 : in std_logic;
    
        micData : in std_logic;
        micClk : out std_logic;
        micLRSel : out std_logic;

        ampPWM : out std_logic;
        ampSD : out std_logic;

        tmpSDA : out std_logic;
        tmpSCL : out std_logic;
        tmpInt : in std_logic;
        tmpCT : in std_logic;
        
        sw : in std_logic_vector(15 downto 0);
        btn : in std_logic_vector(4 downto 0);
        seg_led : out unsigned(31 downto 0);

        viciii_iomode : in std_logic_vector(1 downto 0);
        
        colourram_at_dc00 : in std_logic;
       
        ---------------------------------------------------------------------------
        -- IO port to far call stack
        ---------------------------------------------------------------------------
        farcallstack_we : in std_logic;
        farcallstack_addr : in std_logic_vector(8 downto 0);
        farcallstack_din : in std_logic_vector(63 downto 0);
        farcallstack_dout : out std_logic_vector(63 downto 0)

        );
end iomapper;

architecture behavioral of iomapper is
  component kickstart is
    port (
      Clk : in std_logic;
      address : in std_logic_vector(13 downto 0);
      we : in std_logic;
      cs : in std_logic;
      data_i : in std_logic_vector(7 downto 0);
      data_o : out std_logic_vector(7 downto 0));
  end component;

  component sid6581 is
    port (
      clk_1MHz			: in  std_logic;		-- main SID clock signal
      clk32				: in  std_logic;		-- main clock signal
      reset				: in  std_logic;		-- high active signal (reset when reset = '1')
      cs					: in  std_logic;		-- "chip select", when this signal is '1' this model can be accessed
      we					: in std_logic;		-- when '1' this model can be written to, otherwise access is considered as read
      
      addr				: in  unsigned(4 downto 0);	-- address lines
      di					: in  unsigned(7 downto 0);	-- data in (to chip)
      do					: out unsigned(7 downto 0);	-- data out	(from chip)
      pot_x				: in  unsigned(7 downto 0);	-- paddle input-X
      pot_y				: in  unsigned(7 downto 0);	-- paddle input-Y
      audio_data		: out unsigned(17 downto 0)
      );
  end component;

  component ethernet is
  port (
    clock : in std_logic;
    clock50mhz : in std_logic;
    reset : in std_logic;
    irq : out std_logic := 'Z';

    ---------------------------------------------------------------------------
    -- IO lines to the ethernet controller
    ---------------------------------------------------------------------------
    eth_mdio : inout std_logic;
    eth_mdc : out std_logic;
    eth_reset : out std_logic;
    eth_rxd : in unsigned(1 downto 0);
    eth_txd : out unsigned(1 downto 0);
    eth_txen : out std_logic;
    eth_rxdv : in std_logic;
    eth_rxer : in std_logic;
    eth_interrupt : in std_logic;

    -- Signals from the VIC-IV frame packer to supply the compressed video feed
    buffer_moby_toggle : in std_logic := '0';
    buffer_address : out unsigned(11 downto 0);
    buffer_rdata : in unsigned(7 downto 0);    

    ---------------------------------------------------------------------------
    -- keyboard event capture via ethernet
    ---------------------------------------------------------------------------    
    eth_keycode_toggle : out std_logic;
    eth_keycode : out unsigned(15 downto 0);

    fastio_addr : in unsigned(19 downto 0);
    fastio_write : in std_logic;
    fastio_read : in std_logic;
    fastio_wdata : in unsigned(7 downto 0);
    fastio_rdata : out unsigned(7 downto 0)
    );
  end component;
  
  component sdcardio is
    port (
      clock : in std_logic;
      pixelclk : in std_logic;
      reset : in std_logic;

      ---------------------------------------------------------------------------
      -- fast IO port (clocked at core clock). 1MB address space
      ---------------------------------------------------------------------------
      fastio_addr : in unsigned(19 downto 0);
      fastio_read : in std_logic;
      fastio_write : in std_logic;
      fastio_wdata : in unsigned(7 downto 0);
      fastio_rdata : out unsigned(7 downto 0);

      -- If colour RAM is mapped at $DC00-$DFFF, then don't map sector buffer
      colourram_at_dc00 : in std_logic;
      viciii_iomode : in std_logic_vector(1 downto 0);
      
      sectorbuffermapped : out std_logic;
      sectorbuffermapped2 : out std_logic;
      sectorbuffercs : in std_logic;
      
      led : out std_logic := '0';
      motor : out std_logic := '0';

      -------------------------------------------------------------------------
      -- Lines for the SDcard interface itself
      -------------------------------------------------------------------------
      cs_bo : out std_logic;
      sclk_o : out std_logic;
      mosi_o : out std_logic;
      miso_i : in  std_logic;

      ---------------------------------------------------------------------------
      -- Lines for other devices that we handle here
      ---------------------------------------------------------------------------
      -- Accelerometer
      aclMISO : in std_logic;
      aclMOSI : out std_logic;
      aclSS : out std_logic;
      aclSCK : out std_logic;
      aclInt1 : in std_logic;
      aclInt2 : in std_logic;

      -- Microphone
      micData : in std_logic;
      micClk : out std_logic;
      micLRSel : out std_logic;

      -- Audio in from digital SIDs
      leftsid_audio : in unsigned(17 downto 0);
      rightsid_audio : in unsigned(17 downto 0);

      -- Audio output
      ampPWM : out std_logic;
      ampSD : out std_logic;

      -- Temperature sensor
      tmpSDA : out std_logic;
      tmpSCL : out std_logic;
      tmpInt : in std_logic;
      tmpCT : in std_logic;

      ----------------------------------------------------------------------
      -- Flash RAM for holding config
      ----------------------------------------------------------------------
      QspiSCK : out std_logic;
      QspiDB : inout std_logic_vector(3 downto 0);
      QspiCSn : out std_logic;
      
      last_scan_code : in std_logic_vector(12 downto 0);
      
      -------------------------------------------------------------------------
      -- And general switch inputs on the FPGA board (good as place as any here)
      -------------------------------------------------------------------------
      sw : in std_logic_vector(15 downto 0);
      btn : in std_logic_vector(4 downto 0)

      );
  end component;
  
  component cia6526 is
    port (
      cpuclock : in std_logic;
      phi0 : in std_logic;
      todclock : in std_logic;
      reset : in std_logic;
      irq : out std_logic := '1';

      reg_isr_out : out unsigned(7 downto 0);
      imask_ta_out : out std_logic;

      ---------------------------------------------------------------------------
      -- fast IO port (clocked at core clock). 1MB address space
      ---------------------------------------------------------------------------
      cs : in std_logic;
      fastio_address : in unsigned(7 downto 0);
      fastio_write : in std_logic;
      fastio_wdata : in unsigned(7 downto 0);
      fastio_rdata : out unsigned(7 downto 0);

      portaout : out std_logic_vector(7 downto 0);
      portain : in std_logic_vector(7 downto 0);
      
      portbout : out std_logic_vector(7 downto 0);
      portbin : in std_logic_vector(7 downto 0);

      flagin : in std_logic;

      pcout : out std_logic;

      spout : out std_logic;
      spin : in std_logic;

      countout : out std_logic;
      countin : in std_logic);
  end component;
  component keymapper is    
  port (
    pixelclk : in std_logic;

    last_scan_code : out std_logic_vector(12 downto 0);

    nmi : out std_logic := 'Z';
    reset : out std_logic := 'Z';
    
    -- PS2 keyboard interface
    ps2clock  : in  std_logic;
    ps2data   : in  std_logic;
    -- CIA ports
    porta_in  : in  std_logic_vector(7 downto 0);
    portb_in  : in  std_logic_vector(7 downto 0);
    porta_out : out std_logic_vector(7 downto 0);
    portb_out : out std_logic_vector(7 downto 0);

    ---------------------------------------------------------------------------
    -- keyboard event capture via ethernet
    ---------------------------------------------------------------------------    
    eth_keycode_toggle : in std_logic;
    eth_keycode : in unsigned(15 downto 0)
    
    );
  end component;

  component framepacker is
    port (
      pixelclock : in std_logic;
      ioclock : in std_logic;
      hypervisor_mode : in std_logic;
      
      pixel_stream_in : in unsigned (7 downto 0);
      pixel_y : in unsigned (11 downto 0);
      pixel_valid : in std_logic;
      pixel_newframe : in std_logic;
      pixel_newraster : in std_logic;

      -- Signals for ethernet controller
      buffer_moby_toggle : out std_logic := '0';
      buffer_address : in unsigned(11 downto 0);
      buffer_rdata : out unsigned(7 downto 0);    
      
      ---------------------------------------------------------------------------
      -- fast IO port (clocked at CPU clock).
      ---------------------------------------------------------------------------
      fastio_addr : in unsigned(19 downto 0);
      fastio_write : in std_logic;
      fastio_read : in std_logic;
      fastio_wdata : in unsigned(7 downto 0);
      fastio_rdata : out unsigned(7 downto 0)
      );
  end component;

  component farcallstack IS
  PORT (
    -- CPU fastio port
    clka : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(11 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    -- CPU parallel push/pop port
    clkb : IN STD_LOGIC;
    web : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addrb : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
    dinb : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
    doutb : OUT STD_LOGIC_VECTOR(63 DOWNTO 0)
    );
  end component;
  
  signal kickstartcs : std_logic;

  signal reset_high : std_logic;
  
  signal clock50hz : std_logic := '1';
  constant divisor50hz : integer := 640000; -- 64MHz/50Hz/2;
  signal counter50hz : integer := 0;
  
  signal cia1cs : std_logic;
  signal cia2cs : std_logic;

  signal sectorbuffercs : std_logic;
  signal sector_buffer_mapped_read : std_logic;

  signal farcallstackcs : std_logic;
  signal farcallstack_w : std_logic;
  signal farcallstack_wdata : std_logic_vector(63 downto 0);
  signal farcallstack_rdata : std_logic_vector(63 downto 0);
                        
  signal last_scan_code : std_logic_vector(12 downto 0);
  
  signal cia1porta_out : std_logic_vector(7 downto 0);
  signal cia1porta_in : std_logic_vector(7 downto 0);
  signal cia1portb_out : std_logic_vector(7 downto 0);
  signal cia1portb_in : std_logic_vector(7 downto 0);

  signal leftsid_cs : std_logic;
  signal leftsid_audio : unsigned(17 downto 0);
  signal rightsid_cs : std_logic;
  signal rightsid_audio : unsigned(17 downto 0);

  signal spare_bits : unsigned(4 downto 0);

  signal buffer_moby_toggle : std_logic;
  signal buffer_address : unsigned(11 downto 0);
  signal buffer_rdata : unsigned(7 downto 0);

  signal eth_keycode_toggle : std_logic;
  signal eth_keycode : unsigned(15 downto 0);
  
begin         
  kickstartrom : kickstart port map (
    clk     => clk,
    address => address(13 downto 0),
    we      => w,
    cs      => kickstartcs,
    data_i  => data_i,
    data_o  => data_o);

  framepacker0: framepacker port map (
    ioclock => clk,
    pixelclock => pixelclk,
    hypervisor_mode => cpu_hypervisor_mode,

    pixel_stream_in => pixel_stream_in,
    pixel_y => pixel_y,
    pixel_valid => pixel_valid,
    pixel_newframe => pixel_newframe,
    pixel_newraster => pixel_newraster,

    buffer_moby_toggle => buffer_moby_toggle,
    buffer_address => buffer_address,
    buffer_rdata => buffer_rdata,

    fastio_addr => unsigned(address(19 downto 0)),
    fastio_write => w,
    std_logic_vector(fastio_rdata) => data_o,
    fastio_read => r,
    fastio_wdata => unsigned(data_i)
    );
    
  cia1: cia6526 port map (
    cpuclock => clk,
    phi0 => phi0,
    todclock => clock50hz,
    reset => reset,
    irq => irq,
    reg_isr_out => reg_isr_out,
    imask_ta_out => imask_ta_out,
    cs => cia1cs,
    fastio_address => unsigned(address(7 downto 0)),
    fastio_write => w,
    std_logic_vector(fastio_rdata) => data_o,
    fastio_wdata => unsigned(data_i),
    portaout => cia1porta_out,
    portbout => cia1portb_out,
    portain => cia1porta_in,
    portbin => cia1portb_in,
    flagin => '1',
    spin => '1',
    countin => '1'
    );

  cia2two: cia6526 port map (
    cpuclock => clk,
    phi0 => phi0,
    todclock => clock50hz,
    reset => reset,
    irq => nmi,
    cs => cia2cs,
    fastio_address => unsigned(address(7 downto 0)),
    fastio_write => w,
    std_logic_vector(fastio_rdata) => data_o,
    fastio_wdata => unsigned(data_i),

    -- CIA ports not connected by default
    portbin => x"ff",
    portain => x"ff",
    flagin => '1',
    spin => '1',
    countin => '1'
    );

  keymapper0 : keymapper port map (
    pixelclk       => clk,
    nmi => restore_nmi,
    reset => reset_out,
    ps2clock       => ps2clock,
    ps2data        => ps2data,
    last_scan_code => last_scan_code,
--    key_status     => seg_led(1 downto 0),
    porta_in       => cia1porta_out,
    portb_in       => cia1portb_out,
    porta_out      => cia1porta_in,
    portb_out      => cia1portb_in,

    -- remote keyboard input via ethernet
--    eth_keycode_toggle => eth_keycode_toggle,
--    eth_keycode => eth_keycode

    -- remote 
    eth_keycode_toggle => key_scancode_toggle,
    eth_keycode => key_scancode
);

  leftsid: sid6581 port map (
    clk_1MHz => phi0,
    clk32 => clk,
    reset => reset_high,
    cs => leftsid_cs,
    we => w,
    addr => unsigned(address(4 downto 0)),
    di => unsigned(data_i),
    std_logic_vector(do) => data_o,
    pot_x => x"01",
    pot_y => x"02",
    audio_data => leftsid_audio);

  rightsid: sid6581 port map (
    clk_1MHz => phi0,
    clk32 => clk,
    reset => reset_high,
    cs => rightsid_cs,
    we => w,
    addr => unsigned(address(4 downto 0)),
    di => unsigned(data_i),
    std_logic_vector(do) => data_o,
    pot_x => x"03",
    pot_y => x"04",
    audio_data => rightsid_audio);

  ethernet0 : ethernet port map (
    clock50mhz => clock50mhz,
    clock => clk,
    reset => reset,
    irq => irq,

    ---------------------------------------------------------------------------
    -- IO lines to the ethernet controller
    ---------------------------------------------------------------------------
    eth_mdio => eth_mdio,
    eth_mdc => eth_mdc,
    eth_reset => eth_reset,
    eth_rxd => eth_rxd,
    eth_txd => eth_txd,
    eth_txen => eth_txen,
    eth_rxdv => eth_rxdv,
    eth_rxer => eth_rxer,
    eth_interrupt => eth_interrupt,

    buffer_moby_toggle => buffer_moby_toggle,
    buffer_address => buffer_address,
    buffer_rdata => buffer_rdata,

    eth_keycode_toggle => eth_keycode_toggle,
    eth_keycode => eth_keycode,

    fastio_addr => unsigned(address),
    fastio_write => w,
    fastio_read => r,
    fastio_wdata => unsigned(data_i),
    std_logic_vector(fastio_rdata) => data_o
    );
  
  sdcard0 : sdcardio port map (
    pixelclk => pixelclk,
    clock => clk,
    reset => reset,

    fastio_addr => unsigned(address),
    fastio_write => w,
    fastio_read => r,
    fastio_wdata => unsigned(data_i),
    std_logic_vector(fastio_rdata) => data_o,
    colourram_at_dc00 => colourram_at_dc00,
    viciii_iomode => viciii_iomode,
    sectorbuffermapped => sector_buffer_mapped,
    sectorbuffermapped2 => sector_buffer_mapped_read,
    sectorbuffercs => sectorbuffercs,

    led => led,
    motor => motor,
    
    sw => sw,
    btn => btn,
    cs_bo => cs_bo,
    sclk_o => sclk_o,
    mosi_o => mosi_o,
    miso_i => miso_i,

    aclMISO => aclMISO,
    aclMOSI => aclMOSI,
    aclSS => aclSS,
    aclSCK => aclSCK,
    aclInt1 => aclInt1,
    aclInt2 => aclInt2,
    
    micData => micData,
    micClk => micClk,
    micLRSel => micLRSel,

    leftsid_audio => leftsid_audio,
    rightsid_audio => rightsid_audio,
    
    ampSD => ampSD,
    ampPWM => ampPWM,
    
    tmpSDA => tmpSDA,
    tmpSCL => tmpSCL,
    tmpInt => tmpInt,
    tmpCT => tmpCT,

    QspiSCK => QspiSCK,
    QspiDB => QspiDB,
    QspiCSn => QspiCSn,

    last_scan_code => last_scan_code

    );

  farcallstack0: farcallstack port map (
    clka => clk,
    ena => farcallstackcs,
    wea(0) => w,
    addra => address(11 downto 0),
    dina => data_i,
    douta => data_o,
    clkb => cpuclock,
    web(0) => farcallstack_w,
    addrb => farcallstack_addr,
    dinb => farcallstack_wdata,
    doutb => farcallstack_rdata
    );
  
  process(reset)
  begin
    reset_high <= not reset;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      -- Generate 50Hz signal for TOD clock
      -- (Note that we are a bit conflicted here, as our video mode is PALx4,
      --  but at 60Hz.  We will make our CIAs take 50Hz like in most PAL countries
      -- so that we don't confuse things too much.  We will probably add a 50Hz
      -- raster interrupt filter to help music and games play at the right rate.)
      if counter50hz<divisor50hz then
        counter50hz <= counter50hz + 1;
      else
        clock50hz <= not clock50hz;
        counter50hz <= 0;
      end if;

      seg_led(12) <= eth_keycode_toggle;
      seg_led(11) <= last_scan_code(12);
      seg_led(10 downto 0) <= unsigned(last_scan_code(10 downto 0));
      
    end if;
  end process;
  
  process (r,w,address,cia1portb_in,cia1porta_out,colourram_at_dc00,
           sector_buffer_mapped_read)
  begin  -- process

    if (r or w) = '1' then
      -- @IO:GS $FFF8000-$FFFBFFF 16KB Kickstart/hypervisor ROM
      -- @IO:GS $FFF8000 Hypervisor entry point when $D67F is written
      if address(19 downto 14)&"00" = x"F8" then
        kickstartcs <= cpu_hypervisor_mode;
      else
        kickstartcs <='0';
      end if;

      -- @IO:GS $FFF0000-$FFF0FFF - CPU far call stack (512x8 byte entries)
      if address(19 downto 12) = x"F0" then
        farcallstackcs <= '1';
      else
        farcallstackcs <= '0';
      end if;
      
      -- sdcard sector buffer: only mapped if no colour ram @ $DC00, and if
      -- the sectorbuffer mapping flag is set
      sectorbuffercs <= '0';
      report "fastio address = $" & to_hstring(address) severity note;
      
      if address(19 downto 16) = x"D"
        and address(15 downto 14) = "00"
        and address(11 downto 9)&'0' = x"E"
        and sector_buffer_mapped_read = '1' and colourram_at_dc00 = '0' then
        sectorbuffercs <= '1';
        report "selecting SD card sector buffer" severity note;
      end if;
      -- Also map SD card sector buffer at $FFD6000 - $FFD61FF regardless of
      -- VIC-IV IO mode and mapping of colour RAM
      if address(19 downto 8) = x"D60" or address(19 downto 8) = x"D61" then
        sectorbuffercs <= '1';
      end if;

      -- Now map the SIDs
      -- @IO:C64 $D440-$D47F = left SID
      -- @IO:C64 $D400-$D43F = right SID
      -- @IO:C64 $D480-$D4FF = repeated images of SIDs
      -- Presumably repeated through to $D5FF.  But we will repeat to $D4FF only
      -- so that we can use $D500-$D5FF for other stuff.
      case address(19 downto 8) is
        when x"D04" => leftsid_cs <= address(6); rightsid_cs <= not address(6);
        when x"D14" => leftsid_cs <= address(6); rightsid_cs <= not address(6);
        when x"D24" => leftsid_cs <= address(6); rightsid_cs <= not address(6);
        when x"D34" => leftsid_cs <= address(6); rightsid_cs <= not address(6);
        when others => leftsid_cs <= '0'; rightsid_cs <= '0';
      end case;

      -- $D500 - $D5FF is not currently used.  Probably use some for FPU.
      
      -- $D600 - $D60F is reserved for 6551 serial UART emulation for C65
      -- compatibility (6551 actually only has 4 registers).
      -- 6551 is not currently implemented, so this is just unmapped for now,
      -- except for any read values required to allow the C65 ROM to function.

      -- Hypervisor control (only visible from hypervisor mode) $D640 - $D67F
      -- The hypervisor is a CPU provided function.
      
      -- SD controller and miscellaneous hardware (microphone, accelerometer etc)
      -- uses $D680 - $D6FF
      
      -- CPU uses $FFD{0,1,2,3}700 for DMAgic and other CPU-hosted IO registers.
      
      -- Now map the CIAs.

      -- These are a bit fun, because they only get mapped if colour RAM isn't
      -- being mapped in $DC00-$DFFF using the C65 2K colour ram register
      cia1cs <='0';
      cia2cs <='0';
      if colourram_at_dc00='0' and sector_buffer_mapped_read='0' then
        case address(19 downto 8) is
          when x"D0C" => cia1cs <='1';
          when x"D1C" => cia1cs <='1';
          when x"D2C" => cia1cs <='1';
          when x"D3C" => cia1cs <='1';
          when x"D0D" => cia2cs <='1';
          when x"D1D" => cia2cs <='1';
          when x"D2D" => cia2cs <='1';
          when x"D3D" => cia2cs <='1';
          when others => null;
        end case;
      end if;
    else
      cia1cs <= '0';
      cia2cs <= '0';
      kickstartcs <= '0';
      sectorbuffercs <= '0';
      leftsid_cs <= '0';
      rightsid_cs <= '0';
      farcallstackcs <= '0';
    end if;
  end process;

end behavioral;

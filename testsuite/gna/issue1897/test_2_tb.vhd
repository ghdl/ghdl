--------------------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;

LIBRARY STD;
USE     STD.TEXTIO.ALL;
-- use of vhdl-2008
USE     STD.ENV.FINISH;

LIBRARY WORK;
USE     WORK.ALL;

--------------------------------------------------------------------------------------------

ENTITY test_2_tb IS
END test_2_tb;

--------------------------------------------------------------------------------------------

ARCHITECTURE str_tb OF test_2_tb IS

  constant C_CLK_PERIOD  : time := 10 ns; -- 100 Mhz

  constant C_ADDR_WIDTH  : natural := 32;
  constant C_DATA_WIDTH  : natural := 32;

  
  component wb_master_bfm_v1_0 is
    generic (
     -- Ports of Whisbone Master Bus Interface M00_WB
      G_M00_WB_TARGET_SLAVE_BASEADDR    : std_logic_vector := x"0000_0000";
      G_M00_WB_ADDR_WIDTH               : natural          := C_ADDR_WIDTH;
      G_M00_WB_DATA_WIDTH               : natural          := C_DATA_WIDTH;
                                    
      G_INSTR_FILE                      : string := "./prog.txt"; -- File path which store the transactions to be performed
      G_MMREAD_FILE                     : string := "./read.txt"  -- File path which store the result of the read transactions performed
    );
    port (
    -- Wishbone Clock and Reset signals
       WB_CLK_I           : in  std_logic;
       WB_RSTN_I          : in  std_logic;
    -- Ports of Whisbone Master Bus Interface M00_WB
       M00_WB_ADR_O       : out std_logic_vector(G_M00_WB_ADDR_WIDTH-1 downto 0);
       M00_WB_DAT_O       : out std_logic_vector(G_M00_WB_DATA_WIDTH-1 downto 0);
       M00_WB_DAT_I       : in  std_logic_vector(G_M00_WB_DATA_WIDTH-1 downto 0);
       M00_WB_WE_O        : out std_logic; 
       M00_WB_STB_O       : out std_logic; 
       M00_WB_ACK_I       : in  std_logic; 
       M00_WB_CYC_O       : out std_logic  
    );
  end component;

  signal clk              : std_logic := '1';
  signal rstn             : std_logic := '0';

  signal m00_wb_adr_o     : std_logic_vector(C_ADDR_WIDTH-1 downto 0);
  signal m00_wb_dat_o     : std_logic_vector(C_DATA_WIDTH-1 downto 0);
  signal m00_wb_dat_i     : std_logic_vector(C_DATA_WIDTH-1 downto 0);
  signal m00_wb_we_o      : std_logic;
  signal m00_wb_stb_o     : std_logic;
  signal m00_wb_ack_i     : std_logic;
  signal m00_wb_cyc_o     : std_logic;

BEGIN

  clkGen:
    clk <= not clk after C_CLK_PERIOD/2;

  rstGen :
      rstn <=
      '1' after (1 us);

  dummy_wb_slave:
  process(clk)
  begin
    if rising_edge(clk) then
      m00_wb_ack_i <='0';
      m00_wb_dat_i <= (others=>'-');
      if rstn='1' then
        if m00_wb_stb_o ='1'then
          m00_wb_ack_i <='1';
          m00_wb_dat_i <= x"DEAD_C0DE";
        end if;
      end if;
    end if; -- rising_edge(clk)
  end process;

  wb_bfm_inst: wb_master_bfm_v1_0
    port map(
      WB_CLK_I        => clk              ,
      WB_RSTN_I       => rstn             ,
      M00_WB_ADR_O    => m00_wb_adr_o     ,
      M00_WB_DAT_O    => m00_wb_dat_o     ,
      M00_WB_DAT_I    => m00_wb_dat_i     ,
      M00_WB_WE_O     => m00_wb_we_o      ,
      M00_WB_STB_O    => m00_wb_stb_o     ,
      M00_WB_ACK_I    => m00_wb_ack_i     ,
      M00_WB_CYC_O    => m00_wb_cyc_o
    );

END str_tb;

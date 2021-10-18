
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY wb_master_bfm_v1_0 IS
  GENERIC (
    -- Users to add parameters here

    -- User parameters ends
    -- Do not modify the parameters beyond this line

    -- Ports of Whisbone Master Bus Interface M00_WB
    G_M00_WB_TARGET_SLAVE_BASEADDR    : std_logic_vector     := x"0000_0000"; -- Mem offset
    G_M00_WB_ADDR_WIDTH               : natural              := 32;
    G_M00_WB_DATA_WIDTH               : natural              := 32;

    G_INSTR_FILE                      : string := "./prog.txt"; -- File path which store the transactions to be performed
    G_MMREAD_FILE                     : string := "./read.txt"  -- File path which store the result of the read transactions performed
  );
  PORT (
  -- Users to add ports here

  -- User ports ends
  -- Do not modify the ports beyond this line
  
  -- Wishbone Clock and Reset signals
     WB_CLK_I           : in  std_logic;
     WB_RSTN_I          : in  std_logic;
  -- Ports of Whisbone Master Bus Interface M00_WB
     M00_WB_ADR_O       : out std_logic_vector(G_M00_WB_ADDR_WIDTH-1 downto 0);
     M00_WB_DAT_O       : out std_logic_vector(G_M00_WB_DATA_WIDTH-1 downto 0);
     M00_WB_DAT_I       : in  std_logic_vector(G_M00_WB_DATA_WIDTH-1 downto 0);
     M00_WB_WE_O        : out std_logic; -- Active high, write enable
     M00_WB_STB_O       : out std_logic; -- Active high to start the read/write transaction
     M00_WB_ACK_I       : in  std_logic; -- Active high when a transaction has been sucessfully processed by the slave
     M00_WB_CYC_O       : out std_logic  -- Active high to start a new bus cycle
  );
END wb_master_bfm_v1_0;

ARCHITECTURE arch_imp OF wb_master_bfm_v1_0 IS

  component wb_master_BFM is
    generic (
      G_M00_WB_BASEADDR   : std_logic_vector;
      G_M00_WB_DATA_WIDTH : natural;
      G_M00_WB_ADDR_WIDTH : natural;
      G_INSTR_FILE        : string;
      G_MMREAD_FILE       : string
    );
    port (
     WB_CLK_I           : in  std_logic;
     WB_RSTN_I          : in  std_logic;
     M00_WB_ADR_O       : out std_logic_vector(G_M00_WB_ADDR_WIDTH-1 downto 0);
     M00_WB_DAT_O       : out std_logic_vector(G_M00_WB_DATA_WIDTH-1 downto 0);
     M00_WB_DAT_I       : in  std_logic_vector(G_M00_WB_DATA_WIDTH-1 downto 0);
     M00_WB_WE_O        : out std_logic; -- Active high, write enable
     M00_WB_STB_O       : out std_logic; -- Active high to start the read/write transaction
     M00_WB_ACK_I       : in  std_logic; -- Active high when a transaction has been sucessfully processed by the slave
     M00_WB_CYC_O       : out std_logic  -- Active high to start a new bus cycle
      );
  end component;

BEGIN


-------------------------------------------------------------------------------
-- Master AXILITE
-------------------------------------------------------------------------------

wb_master_BFM_inst: entity work.wb_master_bfm
  generic map (
     G_M00_WB_BASEADDR   => G_M00_WB_TARGET_SLAVE_BASEADDR ,
     G_M00_WB_DATA_WIDTH => G_M00_WB_DATA_WIDTH  ,
     G_M00_WB_ADDR_WIDTH => G_M00_WB_ADDR_WIDTH  ,
     G_INSTR_FILE        => G_INSTR_FILE         ,
     G_MMREAD_FILE       => G_MMREAD_FILE
   )
  port map (
     WB_CLK_I        => WB_CLK_I         ,
     WB_RSTN_I       => WB_RSTN_I        ,
     M00_WB_ADR_O    => M00_WB_ADR_O     ,
     M00_WB_DAT_O    => M00_WB_DAT_O     ,
     M00_WB_DAT_I    => M00_WB_DAT_I     ,
     M00_WB_WE_O     => M00_WB_WE_O      ,
     M00_WB_STB_O    => M00_WB_STB_O     ,
     M00_WB_ACK_I    => M00_WB_ACK_I     ,
     M00_WB_CYC_O    => M00_WB_CYC_O
   );

END arch_imp;

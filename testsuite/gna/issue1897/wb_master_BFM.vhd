


-----------------------------------------------------------------------------------
-- * Libs
-----------------------------------------------------------------------------------
LIBRARY IEEE                          ;
USE     IEEE.STD_LOGIC_1164.ALL       ;
USE     IEEE.NUMERIC_STD.ALL          ;

LIBRARY STD                           ;
USE     STD.TEXTIO.ALL                ;

-- USE OF VHDL-2008
USE     STD.ENV.FINISH                ;
-----------------------------------------------------------------------------------
-- * Entity
-----------------------------------------------------------------------------------
ENTITY wb_master_bfm IS
   GENERIC (
     G_M00_WB_BASEADDR   : std_logic_vector:= x"A000_0000";
     G_M00_WB_DATA_WIDTH : integer := 32;
     G_M00_WB_ADDR_WIDTH : integer := 32;
     G_INSTR_FILE        : string  := "../prog.txt";
     G_MMREAD_FILE       : string  := "../results.txt"
   );
   PORT (
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

END wb_master_bfm;

-----------------------------------------------------------------------------------
-- * Architecture Begins
-----------------------------------------------------------------------------------
ARCHITECTURE behavioural OF wb_master_bfm2 IS
begin
-----------------------------------------------------------------------------------
-- * Architecture Ends
-----------------------------------------------------------------------------------
END ARCHITECTURE behavioural;


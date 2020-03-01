library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;
    use IEEE.MATH_REAL.all;

--library ghdl_test;
--    use ghdl_test.crc_pkg.all;
    use work.crc_pkg.all;

entity wbCrc_syn is 
    generic(  yes_G        : std_logic :='1'
            ; fClkSys_Hz_G : integer   := 100e6
            ; isSimul_G    : boolean   := false
            ; wbDataWidth_G : integer   :=  8   
            ; wbAddrWidth_G : integer   := 32  
            ; wbTgaWidth_G  : integer   :=  1 
            ; wbTgcWidth_G  : integer   :=  1 
            ; wbTgdWidth_G  : integer   :=  1 
            ; crcDefault_G  : String    := "CRC-32/CCITT-FALSE"   
           );
    port( clk_i         : in    std_ulogic; 
    
          rx_is_dat_i   : in    std_ulogic_vector( wbDataWidth_G     - 1 downto 0 );
          rx_is_adr_i   : in    std_ulogic_vector( wbAddrWidth_G     - 1 downto 0 );
          rx_is_sel_i   : in    std_ulogic_vector( wbDataWidth_G / 8 - 1 downto 0 );
          rx_is_loc_i   : in    std_ulogic;
          rx_is_cyc_i   : in    std_ulogic;
          rx_is_stb_i   : in    std_ulogic;
          rx_is_we_i    : in    std_ulogic;
          rx_os_ack_o   :   out std_ulogic;
          rx_os_stl_o   :   out std_ulogic;
          rx_os_err_o   :   out std_ulogic;
          
          tx_os_cyc_o  :    out std_ulogic;
          tx_os_stb_o  :    out std_ulogic;
          tx_os_we_o   :    out std_ulogic;
          tx_os_dat_o  :    out std_ulogic_vector( getCrc32Param( crcDefault_G , 8 ).poly'length - 1 downto 0 );
          tx_os_cti_o  :    out std_ulogic_vector(2 downto 0) ;
          tx_os_sel_o  :    out std_ulogic_vector( wbDataWidth_G / 8 - 1 downto 0 );
          tx_os_loc_o  :    out std_ulogic;
          tx_is_ack_i  : in     std_ulogic
        );
end wbCrc_syn;
architecture rtl of wbCrc_syn  is  
begin 

end rtl;

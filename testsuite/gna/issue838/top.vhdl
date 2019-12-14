library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fpga_avalon_top is
    port(    
    -- Zynq Clock/Reset    
    pl_fclk                   :in std_logic;
    pl_areset_n               :in std_logic;  
    
    -- AVALON S-Port
    s_amm_wait                :out std_logic; 
    s_amm_addr                :in  std_logic_vector(31 downto 0);
    s_amm_byte_en             :in  std_logic_vector(1 downto 0);
    s_amm_rd_en               :in  std_logic;
    s_amm_rd_data             :out std_logic_vector(31 downto 0);
    s_amm_rd_valid            :out std_logic;
    s_amm_wr_en               :in  std_logic; 
    s_amm_wr_data             :in  std_logic_vector(31 downto 0); 
    s_amm_wr_resvld           :out std_logic; 
    s_amm_response            :out std_logic_vector(1 downto 0);
        
    -- AVALON M-Port
    m_amm_wait                :in  std_logic; 
    m_amm_addr                :out std_logic_vector(31 downto 0);
    m_amm_rd_en               :out std_logic;
    m_amm_rd_data             :in  std_logic_vector(31 downto 0);
    m_amm_rd_valid            :in  std_logic;
    m_amm_wr_en               :out std_logic; 
    m_amm_wr_data             :out std_logic_vector(31 downto 0)    
);

end entity;

architecture rtl of fpga_avalon_top is

-- UNCOMMENT WHEN ADDING TO VIVADO, COMMENT-OUT WHEN SIMULATING IN GHDL:
ATTRIBUTE X_INTERFACE_INFO : STRING;    
ATTRIBUTE X_INTERFACE_INFO of s_amm_wait          :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm WAITREQUEST";
ATTRIBUTE X_INTERFACE_INFO of s_amm_addr          :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm ADDRESS";
ATTRIBUTE X_INTERFACE_INFO of s_amm_byte_en       :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm BYTEENABLE";
ATTRIBUTE X_INTERFACE_INFO of s_amm_rd_en         :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm READ";  
ATTRIBUTE X_INTERFACE_INFO of s_amm_rd_data       :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm READDATA";
ATTRIBUTE X_INTERFACE_INFO of s_amm_rd_valid      :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm READDATAVALID";
ATTRIBUTE X_INTERFACE_INFO of s_amm_wr_en         :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm WRITE";
ATTRIBUTE X_INTERFACE_INFO of s_amm_wr_data       :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm WRITEDATA";
ATTRIBUTE X_INTERFACE_INFO of s_amm_wr_resvld     :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm WRITERESPONSEVALID";
ATTRIBUTE X_INTERFACE_INFO of s_amm_response      :SIGNAL is "xilinx.com:interface:avalon:1.0 s_amm RESPONSE";
ATTRIBUTE X_INTERFACE_INFO of m_amm_wait          :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm WAITREQUEST";
ATTRIBUTE X_INTERFACE_INFO of m_amm_addr          :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm ADDRESS";
ATTRIBUTE X_INTERFACE_INFO of m_amm_rd_en         :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm READ";  
ATTRIBUTE X_INTERFACE_INFO of m_amm_rd_data       :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm READDATA";
ATTRIBUTE X_INTERFACE_INFO of m_amm_rd_valid      :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm READDATAVALID";
ATTRIBUTE X_INTERFACE_INFO of m_amm_wr_en         :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm WRITE";
ATTRIBUTE X_INTERFACE_INFO of m_amm_wr_data       :SIGNAL is "xilinx.com:interface:avalon:1.0 m_amm WRITEDATA";
begin

end architecture;

-- top_capitalisation.vhd
----------------------------------------------------------------------

-- top_capitalisation
-- |
-- + capitalisation
-- + UART_8N1_RX
-- + UART_8N1_TX

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top_capitalisation is
  generic (clk_freq : integer;
           baudrate : integer);
  port(
    board_clk     : in std_logic ;
    reset         : in std_logic ;
    rx            : in std_logic ;
    tx            : out std_logic );
end top_capitalisation;

architecture behavior of top_capitalisation is
  signal rd_en   : std_logic;
  signal rdata   : std_logic_vector (7 downto 0);
  signal rd      : std_logic; 

  signal wr_en   : std_logic;
  signal wdata   : std_logic_vector (7 downto 0);
  signal wr      : std_logic;

component capitalisation is
  port(
    clk        : in  std_logic;
    reset      : in  std_logic;
    --in
    rdata      : in  std_logic_vector(7 downto 0);
    rd_en      : in  std_logic;
    rd         : out std_logic;
    --out
    wdata      : out std_logic_vector(7 downto 0);
    wr_en      : in  std_logic;
    wr         : out std_logic
    );
end component;

component UART_8N1_RX is
  generic (clk_freq : integer;
           baudrate : integer);
  port(
    clk           : in  std_logic;
    reset         : in  std_logic;
    --8bit interface
    rdata         : out std_logic_vector(7 downto 0);
    rd            : in  std_logic;
    rd_en         : out std_logic;
    --physical wire RX
    rx            : in  std_logic
    );
end component; 

component UART_8N1_TX is
  generic(
    clk_freq   : integer;
    baudrate   : integer;
    addr_depth : integer:=5); 
  port(
    clk        : in std_logic;
    reset      : in std_logic;
    --8bit  interface
    wdata      : in  std_logic_vector(7 downto 0);
    wr         : in  std_logic;
    wr_en      : out std_logic;
    
    --physical wire
    tx         : out std_logic);
end component;


begin

UART_RX:UART_8N1_RX 
  generic map (
    clk_freq   => clk_freq,
    baudrate   =>115200)
  port map(
    clk           => board_clk,
    reset         => reset,
    rdata         => rdata,
    rd            => rd,
    rd_en         => rd_en,
    --physical wire RX
    rx            => rx
    );

trans: capitalisation 
  port map(
    clk        => board_clk,
    reset      => reset,
    --in
    rdata      => rdata,
    rd_en      => rd_en,
    rd         => rd,
    --out
    wdata      => wdata,
    wr_en      => wr_en,
    wr         => wr
    );

UART_TX: UART_8N1_TX 
  generic map(
    clk_freq   => clk_freq,
    baudrate   => 115200) 
  port map(
    clk        => board_clk,
    reset      => reset,
    --8bit  interface
    wdata      => wdata,
    wr         => wr,
    wr_en      => wr_en,
    
    --physical wire TX
    tx         => tx
    );
end;

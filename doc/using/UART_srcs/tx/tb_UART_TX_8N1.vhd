-- tb_UART_TX_8N1.vhd
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_UART_TX_8N1 is
end tb_UART_TX_8N1;

architecture behavior of tb_UART_TX_8N1 is

  --Inputs
  signal board_clk : std_logic := '0';
  signal reset     : std_logic := '0';
  signal send_data : std_logic_vector (7 downto 0);
  signal wr        : std_logic := '0';
  signal tx        : std_logic;
  
  -- Component Declaration for the Unit Under Test (UUT)
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

  constant board_clk_period : time := 10 ns;
  constant board_clk_freq: integer :=100E6;   --100MHz
  
begin

  -- Instantiate the Unit Under Test (UUT)
UART_TX: UART_8N1_TX 
  generic map(
    clk_freq   => board_clk_freq,
    baudrate   => 115200
  --  addr_depth => use predefined
  ) 
  port map(
    clk        => board_clk,
    reset      => reset,

    wdata      => send_data,
    wr         => wr,
    wr_en      => open,

    tx         => tx
    );

    -- Clock process definitions
    board_clk_process : process
      begin
          board_clk <= '0';
        wait for board_clk_period/2;
          board_clk <= '1';
        wait for board_clk_period/2;
      end process;


   -- Stimulus process
   stim_proc : process
     begin
       reset <= '1';

      wait for 15 us;
        reset <='0';   
        send_data<=X"A0";
      wait until rising_edge(board_clk);
        wr<= '1';

      wait until rising_edge(board_clk);
        wr<= '0';
        
      wait for 5 us;

      wait until rising_edge(board_clk);
        wr<= '1';
        send_data<=X"B1"; 
                          
      wait until rising_edge(board_clk);
        wr<= '0';
      wait;
    end process;

   end;

-- tb_UART_RX_8N1.vhd
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity tb_UART_RX_8N1 is
end tb_UART_RX_8N1;

architecture behavior of tb_UART_RX_8N1 is

  signal board_clk     : std_logic := '0';
  signal reset         : std_logic := '0';
  signal rx            : std_logic := '1';
  signal rd            : std_logic := '0';
  signal rd_en     : std_logic;
  signal data          : std_logic_vector (7 downto 0);
 
  procedure tx_char (signal txpin : out std_logic; 
              txdata : in character; baudrate : in integer) is
    constant bittime : time := (integer(1000000000.0/real(baudrate))) * 1 ns; 
    variable c       : std_logic_vector(7 downto 0);
  begin
    c     := std_logic_vector(to_unsigned(character'pos(txdata), 8));
    txpin <= '0';                       -- Startbit
    wait for bittime;
    for i in 0 to 7 loop
      txpin <= c(i);
      wait for bittime;
    end loop;
    txpin <= '1';                       -- Stopbit
    wait for bittime;
  end tx_char;
  --Outputs

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
  
  constant board_clk_period : time := 10 ns;
  constant board_clk_freq: integer :=100E6;   --100MHz
  
begin

  -- Instantiate the Unit Under Test (UUT)
UART_RX:UART_8N1_RX 

  generic map (
    clk_freq   => board_clk_freq,
    baudrate   =>115200)
  port map(
    clk           => board_clk,
    reset         => reset,
    rdata         => data,
    rd            => rd,
    rd_en         => rd_en,
    --physical wire RX
    rx            => rx
    );

  process 
  begin
    wait for 80000 ns;
      tx_char(RX, '$', 115200);
      tx_char(RX, 'g', 115200); 
      tx_char(RX, '#', 115200); 
    wait for 50 us;
      tx_char(RX, '6', 115200);                         
      tx_char(RX, '7', 115200); 
   wait;                       -- will wait forever
   end process;
          
  -- Clock process definitions
  board_clk_process : process
  begin
    board_clk <= '0';
    wait for board_clk_period/2;
      board_clk <= '1';
    wait for board_clk_period/2;
  end process;

  -- Stimulus process
  process
     begin
         reset <= '1';
       wait for 100 ns;
        reset <='0';   
      wait;
    end process;

   -- Stimulus process
   stim_proc : process
   begin 
 
   loop
      wait for 200 us;                     
      wait until rising_edge(board_clk);
      if rd_en='1' then 
        rd<= '1';
      end if;
      wait until rising_edge(board_clk);
       rd<= '0';        
    end loop;

      wait;
    end process;

   end;

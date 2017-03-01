-- tb_capitalisation.vhd
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_capitalisation is
end tb_capitalisation;

architecture behavior of tb_capitalisation is
  
  constant board_clk_period : time := 10 ns;
  constant board_clk_freq: integer :=100E6;   --100MHz

  signal board_clk     : std_logic := '0';
  signal reset         : std_logic := '0';
  signal rx            : std_logic := '1';
  signal tx            : std_logic;

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

  -- Component Declaration for the Unit Under Test (UUT) 
component top_capitalisation is
  generic (clk_freq : integer;
           baudrate : integer);
  port(
    board_clk     : in std_logic ;
    reset         : in std_logic ;
    rx            : in std_logic ;
    tx            : out std_logic );
end component;

begin
  -- Instantiate the Unit Under Test (UUT)
UART_example: top_capitalisation 
  generic map (
    clk_freq   => board_clk_freq,
    baudrate   =>115200)           
  port map(
    board_clk     => board_clk,
    reset         => reset,
    rx            => rx,
    tx            => tx );

  process 
  begin
    wait for 80000 ns;
      tx_char(RX, '$', 115200);
      tx_char(RX, 'g', 115200); 
      tx_char(RX, '#', 115200); 
    wait for 50 us;
      tx_char(RX, 'b', 115200);                         
      tx_char(RX, 'c', 115200); 
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
 
end;

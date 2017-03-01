-- UART_RX_8N1.vhd
----------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;


entity UART_8N1_RX is

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
end UART_8N1_RX;


architecture Behavioral of UART_8N1_RX is

  type   state_type is (idle, start, data0, data1, data2, data3, data4,
   data5, data6, data7, stop);
  signal state : state_type;

  signal puffer : std_logic_vector (7 downto 0);

--FIFO
  type RAM is array (0 to 63) of std_logic_vector (7 downto 0);

  signal fifo : RAM ;

  signal nextwrite    : unsigned(5 downto 0);
  signal nextread     : unsigned(5 downto 0);

  constant tick       : integer := clk_freq/baudrate;
  signal tick_counter : integer range 0 to (tick+1);
 

begin
  
  rdata  <= fifo(to_integer(nextread));
  
  process (clk)
  begin
    if rising_edge(clk) then
      if rd = '1'  then
        nextread <= nextread+1;
      end if;
      if reset = '1' then 
        nextread <= (others => '0');
      end if;			  
    end if;
  end process;

 rd_en<= '0' when  nextread=nextwrite else '1';
 

  process(clk)
  begin
    
    if (clk'event and clk = '1') then
      tick_counter <= tick_counter + 1;

      case state is
        
        when idle =>
          tick_counter <= 0;
          if (rx = '0') then  --check start condtion
            state <= start;
          else
            state <= idle;
          end if;
            
        when start =>
          if (tick_counter = tick/2) then  --capture in the middle
            tick_counter <= 0;
            state        <= data0;
          end if;
          
        when data0 =>
          if (tick_counter = tick) then
            puffer (0)   <= rx;
            tick_counter <= 0;
            state        <= data1;
          end if;
        when data1 =>
          if (tick_counter = tick) then
            puffer (1)   <= rx;
            tick_counter <= 0;
            state        <= data2;
          end if;
        when data2 =>
          if (tick_counter = tick) then
            puffer (2)   <= rx;
            tick_counter <= 0;
            state        <= data3;
          end if;
        when data3 =>
          if (tick_counter = tick) then
            puffer(3)    <= rx;
            tick_counter <= 0;
            state        <= data4;
          end if;
        when data4 =>
          if (tick_counter = tick) then
            puffer (4)   <= rx;
            tick_counter <= 0;
            state        <= data5;
          end if;
        when data5 =>
          if (tick_counter = tick) then
            puffer (5)   <= rx;
            tick_counter <= 0;
            state        <= data6;
          end if;
        when data6 =>
          if (tick_counter = tick) then
            puffer (6)   <= rx;
            tick_counter <= 0;
            state        <= data7;
          end if;
        when data7 =>
          if (tick_counter = tick) then
            puffer (7)   <= rx;
            tick_counter <= 0;
            state        <= stop;
          end if;
        when stop =>
          if (tick_counter = tick) then
            fifo(to_integer(nextwrite)) <= puffer;
            nextwrite                   <= nextwrite+1;
            tick_counter <= 0;
            state        <= idle;
          end if;
      end case;
      if reset='1' then
        state <=idle;
        nextwrite <= (others => '0');
      end if;
    end if;

  end process;
end Behavioral;


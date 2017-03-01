-- UART_TX_8N1.vhd
----------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

--UART transmiter without parity

entity UART_8N1_TX is
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
end;


architecture Behavioral of UART_8N1_TX is
--FSM
  type   state_type is (idle, start, data0, data1, data2, data3, data4,
   data5, data6, data7, stop);
   
  signal state     : state_type := idle;
  signal nextstate : state_type := idle;
  
  --FIFO
  type   RAM is array (0 to (2**(addr_depth)-1)) of std_logic_vector (7 downto 0);
  signal fifo      : RAM;
  signal nextwrite : unsigned((addr_depth-1) downto 0);
  signal nextread  : unsigned((addr_depth-1) downto 0);
   
  signal send_empty: std_logic;

  --output
  signal   data_tx      : std_logic_vector (7 downto 0);
  constant tick         : integer := (clk_freq/baudrate);
  signal   tick_counter : integer range 0 to (tick+1);

begin

  wr_en  <= '0' when (nextwrite+1 = nextread) else '1'; 
  send_empty<='1' when nextwrite=nextread else '0';
  
  process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        nextread <= (others => '0');
      elsif state = stop and nextstate = idle then
        nextread <= nextread+1;
      end if;
    end if;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      state <= nextstate;
    end if;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        nextwrite <= (others => '0');
      elsif wr = '1' then
        fifo(to_integer(nextwrite)) <= wdata;
        nextwrite                   <= nextwrite+1;
        
      end if;
    end if;
  end process;


  data_tx  <= fifo(to_integer(nextread));

  process(clk)
  begin
    if rising_edge(clk) then
      if state = idle then
        if nextwrite /= nextread then
          nextstate    <= start;
          tick_counter <= 0;
          tx           <= '0';
        else
          tx <= '1';
        end if;
      else
        if tick_counter = tick then
          tick_counter <= 0;
        else
          tick_counter <= tick_counter + 1;
        end if;
      end if;
      if tick_counter = tick then
        if state = start then
          nextstate <= data0;
          tx        <= data_tx(0);
        end if;

        if state = data0 then
          nextstate <= data1;
          tx        <= data_tx(1);
        end if;

        if state = data1 then
          nextstate <= data2;
          tx        <= data_tx(2);
        end if;

        if state = data2 then
          nextstate <= data3;
          tx        <= data_tx(3);
        end if;
        if state = data3 then
          nextstate <= data4;
          tx        <= data_tx(4);
        end if;
        if state = data4 then
          nextstate <= data5;
          tx        <= data_tx(5);
        end if;
        if state = data5 then
          nextstate <= data6;
          tx        <= data_tx(6);
        end if;
        if state = data6 then
          nextstate <= data7;
          tx        <= data_tx(7);
        end if;
        if state = data7 then
          nextstate <= stop;
          tx        <= '1';
        end if;
        if state = stop then
          nextstate <= idle;
          tx        <= '1';
        end if;
      end if;

      if reset = '1' then
        tick_counter <= 0;
        tx           <= '1';
        nextstate    <= idle;
      end if;
    end if;

  end process;
  

end Behavioral;


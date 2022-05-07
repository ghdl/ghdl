library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity generic_fifo_fwft is 
  generic (
    type stream_t;
    size : integer := 256;
    async_reset : boolean := false
  );
  port (
         clk     : in  std_logic;
         rst     : in  std_logic;
         datain  : in  stream_t;
         dataout : out stream_t;
         empty   : out std_logic;
         full    : out std_logic;
         wr      : in  std_logic;
         rd      : in  std_logic
       );
end;
architecture a_generic_fifo_fwft of generic_fifo_fwft is 
  type memory_t is array(size-1 downto 0) of stream_t;
  signal wrptr : integer range 0 to size - 1;
  signal rdptr : integer range 0 to size - 1;
  signal mem : memory_t;
  signal inverted : boolean;
begin

  empty <= '1' when (rdptr = wrptr) and not inverted else '0';
  full  <= '1' when (rdptr = wrptr) and     inverted else '0';

  dataout <= mem(rdptr);

  process (all) is
  begin
    if rising_edge(clk) then
      if wr and not full then
        mem(wrptr) <= datain;
        wrptr <= wrptr + 1;
      end if;
      if rd and not empty then
        rdptr <= rdptr + 1;
      end if;
      if wr and rd then
        null;
      elsif wr and not full then
        inverted <= not inverted when wrptr + 1 mod size < wrptr;
      elsif rd and not empty then
        inverted <= not inverted when rdptr + 1 mod size < rdptr;
      end if;
      if not async_reset then
        if rst then
          inverted <= false;
          rdptr <= 0;
          wrptr <= 0;
        end if;
      end if;
    end if;
    if async_reset then
      if rst then
        inverted <= false;
        rdptr <= 0;
        wrptr <= 0;
      end if;
    end if;
  end process;
end;

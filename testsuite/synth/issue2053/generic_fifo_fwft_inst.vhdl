library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

--use work.utils.all;

entity generic_fifo_fwft_inst is 
  port (
         clk     : in  std_logic;
         rst     : in  std_logic;
         datain  : in  std_logic_vector(7 downto 0);
         dataout : out std_logic_vector(7 downto 0);
         empty   : out std_logic;
         full    : out std_logic;
         wr      : in  std_logic;
         rd      : in  std_logic
       );
end;

architecture a_generic_fifo_fwft_inst of generic_fifo_fwft_inst is 
  type mystream_t is record
    x : std_logic_vector(3 downto 0);
    y : integer range 0 to 2**4-1;
  end record;
  signal imin  : mystream_t;
  signal imout : mystream_t;
begin

  dataout <= imin.x & std_logic_vector(to_unsigned(imin.y, 4));
  imin.x <= datain(7 downto 4);
  imin.y <= to_integer(unsigned(datain(3 downto 0)));


fifo: entity work.generic_fifo_fwft 
  generic map (
    stream_t    => mystream_t,
    size        => 256,
    async_reset => false
  )
  port map (
         clk     => clk,
         rst     => rst,
         datain  => imin,
         dataout => imout,
         empty   => empty,
         full    => full,
         wr      => wr,
         rd      => rd
       );
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity level2 is
   generic (
      G_ADDR_SIZE : integer;
      G_DATA_SIZE : integer
   );
   port (
      clk_i         : in  std_logic;
      rst_i         : in  std_logic;
      -- Read interface
      rd_addr_i     : in  std_logic_vector(G_ADDR_SIZE-1 downto 0);
      rd_data_o     : out std_logic_vector(G_DATA_SIZE-1 downto 0);
      -- Write interface
      wr_addr_i     : in  std_logic_vector(G_ADDR_SIZE-1 downto 0);
      wr_data_i     : in  std_logic_vector(G_DATA_SIZE-1 downto 0);
      wr_en_i       : in  std_logic
   );
end entity level2;

architecture synthesis of level2 is

begin

end architecture synthesis;

-- loopback engine
----------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

entity capitalisation is
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
end;

architecture Behavioral of capitalisation is

 

begin


  process(clk)
  begin
      wr<='0';
      rd<='0';
      if wr_en='1' and rd_en ='1' then
         wr<='1';
         rd<='1';
        if (unsigned(rdata)>X"60") and 
           (unsigned(rdata)<X"7B") then
             wdata<=rdata(7 downto 6 )&'0'& rdata(4 downto 0);
        else
          wdata<=rdata;
        end if;
      end if;
      if reset='1' then
        wr<='0';
      end if;              
  end process;

 

end Behavioral;


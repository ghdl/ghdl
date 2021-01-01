-- ent.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;
   
entity ent is
   port (
      clk_i  : in  std_logic;
      done_o : out std_logic
   ); 
end entity ent;
   
architecture synthesis of ent is
   signal u0 : std_logic_vector(2 downto 0) := "101";
begin
   done_o <= '0';
end architecture synthesis;

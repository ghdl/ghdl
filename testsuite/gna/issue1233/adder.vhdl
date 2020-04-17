library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity adder is
   port
   (
      nibble1, nibble2 : in unsigned(3 downto 0); 
      sum       : out unsigned(3 downto 0); 
      carry_out : out std_logic
   );
end entity adder;
 
architecture behavioral of adder is
   signal temp : unsigned(4 downto 0); 
begin 
   temp <= ("0" & nibble1) + nibble2; 
   sum       <= temp(3 downto 0); 
   carry_out <= temp(4);
end architecture behavioral;


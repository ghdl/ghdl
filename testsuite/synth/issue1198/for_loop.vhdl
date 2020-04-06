library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity for_loop is
 port(
  a     : in  std_logic_vector(7 downto 0);
  Count : out std_logic_vector(2 downto 0)
 );
end for_loop;

architecture behavior of for_loop is
begin
 process(a)
  variable Count_Aux : std_logic_vector(2 downto 0);
 begin
  Count_Aux := "000";
  for i in a'range loop
   if (a(i) = '0') then
    Count_Aux := Count_Aux + 1;
   end if;
  end loop;
  Count <= Count_Aux;
 end process;
end behavior;

library ieee;
use ieee.std_logic_1164.all;

entity latches is
 port(
  G, D, CLR : in  std_logic;
  Q         : out std_logic
 );
end latches;

architecture archi of latches is
begin
 process(CLR, D, G)
 begin
  if (CLR = '1') then
   Q <= '0';
  elsif (G = '1') then
   Q <= D;
  end if;
 end process;
end archi;

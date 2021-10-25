library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity assert2 is
 port (clk, rst: std_logic;
       cnt : out unsigned(3 downto 0));
end assert2;

architecture behav of assert2 is
 signal val : unsigned (3 downto 0);
 function incr (a : integer) return integer is
 begin
   return a + 1;
 end function incr;
begin
 process(clk)
 begin
   if rising_edge(clk) then
     if rst = '1' then
       val <= (others => '0');
     else
       val <= val + 1;
     end if;
   end if;
 end process;
 cnt <= val;
end behav;

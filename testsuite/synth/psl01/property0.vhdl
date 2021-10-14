library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity property0 is
 port (clk, rst: std_logic;
       cnt : out unsigned(3 downto 0));
end property0;

architecture behav of property0 is
 signal val : unsigned (3 downto 0);
 default clock is rising_edge(clk);
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

 property prop is always {val = 8} |=> {val = 9} abort rst;

 assert prop;

end behav;
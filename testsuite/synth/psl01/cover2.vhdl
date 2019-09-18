library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover2 is
 port (clk, rst: std_logic;
       cnt : out unsigned(3 downto 0));
end cover2;

architecture behav of cover2 is
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

 cover {val = 10};
end behav;

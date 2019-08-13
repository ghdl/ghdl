library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity hello is
 port (clk, rst: std_logic;
       cnt : out unsigned(3 downto 0));
end hello;

architecture behav of hello is
 signal val : unsigned (3 downto 0);
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

 --psl default clock is clk;
 --psl restrict {rst; (not rst)[*]};
 --psl assert always val /= 5 or rst = '1';
 --psl assume always val < 50;
end behav;

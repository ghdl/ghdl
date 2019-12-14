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

 --psl default clock is rising_edge(clk);
 --psl restrict {rst; (not rst)[*]};
 --psl assert always val /= 5 abort rst;
 --psl assume always val < 10;
 --psl cover {val = 10};
end behav;

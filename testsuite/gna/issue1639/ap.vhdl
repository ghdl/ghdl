entity ap_a_04 is

 end entity ap_a_04;

 library ieee;  use ieee.std_logic_1164.all;

 architecture test of ap_a_04 is

   signal a, b, y : std_ulogic;

 begin

   -- code from book

   y <= a or b;

   -- end code from book

   a <= '0', '1' after 10 ns;
   b <= '0', '1' after 9223372036854775802 ns, '0' after 10 ns, '1' after 15 ns;

 end architecture test;

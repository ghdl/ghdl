
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_test is end;

architecture arch_tb of tb_test is
   signal reset_s, clk_s : std_logic;
   signal i_s : integer;
   signal u_s : unsigned(7 downto 0);
begin

   reset : reset_s <= '0',
                      '1' after 20 ns,
                      '0' after 400 ns;

   clock : process begin
      clk_s <= '0';
      wait for 100 ns;
      clk_s <= '1';
      wait for 100 ns;
   end process clock;

   process (reset_s, clk_s) begin
      if reset_s = '1' then
         report integer'image(i_s); -- Will report -2147483648
         i_s <= 0;
      elsif rising_edge(clk_s) then
         i_s <= 3;
      end if;
   end process;

   u_s <= to_unsigned(i_s, 8); -- Will give a bound check failure

end architecture arch_tb;


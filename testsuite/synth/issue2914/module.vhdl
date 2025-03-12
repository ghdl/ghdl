library ieee;
use ieee.std_logic_1164.all;

entity module is
   port (
      clk_i    : in  std_logic;
      input_i  : in  std_logic;
      output_o : out std_logic
      );
end entity module;

architecture rtl of module is
begin
   b_block : block
      signal my_signal : std_logic;
   begin
      process(clk_i)
      begin
         if rising_edge(clk_i) then
            my_signal <= input_i;
         end if;
      end process;
      output_o <= my_signal;
   end block;
end architecture;

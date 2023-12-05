library ieee;
use ieee.std_logic_1164.all;

entity test_func is
   port (
      clk_i    : in  std_logic;
      input_i  : in  std_logic_vector(15 downto 0);
      output_o : out std_logic_vector(3 downto 0)
      );
end entity test_func;

architecture rtl of test_func is

   function f_my_function (
      c_input : std_logic_vector)
      return std_logic_vector is
   begin
      for i in 0 to c_input'length-1 loop
         if c_input(i) = '1' then
            return x"1";
         end if;
      end loop;
      return x"0";
   end function;

begin

   process(clk_i)
   begin
      if rising_edge(clk_i) then
         output_o <= f_my_function(input_i);
      end if;
   end process;

end architecture;

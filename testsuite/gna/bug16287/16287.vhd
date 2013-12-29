library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end test;

architecture a of test is

signal bt_addr : unsigned(6 downto 0) := "1010101";
alias dw_addr : unsigned(4 downto 0) is bt_addr(6 downto 2 ) ;

signal s64 : std_logic_vector(63 downto 0) := (others => '0');
alias a32 : std_logic_vector(31 downto 0) is s64(31 downto 0); 

begin
   s64 <= X"fedcba9876543210";

   dw_addr <= "00000", "01010" after 1 ns, "11111" after 2 ns;

   process(a32, bt_addr) is
   begin
      report "A32 = " & integer'image(to_integer(unsigned(a32))) severity note;
      report "bt_addr = " & integer'image(to_integer(bt_addr)) severity note;
   end process;

end a;

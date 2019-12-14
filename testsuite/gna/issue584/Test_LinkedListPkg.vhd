library ieee;
use ieee.std_logic_1164.all;
library std;
use std.textio.all;


entity test_linkedListPkg is
  package NaturalLinkedListPackage is new work.LinkedListPkg generic map(natural);
  use NaturalLinkedListPackage.all;
end entity test_linkedListPkg;

architecture behavioral of test_linkedListPkg is
  
begin
   process
     variable list : LinkedListPtr;
     variable l : line;
     variable cnt : natural;
   begin
     write (l, cnt);
     writeline (output, l);
     wait;
   end process;  

end architecture behavioral;
                                      

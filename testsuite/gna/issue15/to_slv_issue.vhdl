library ieee ; 
use ieee.numeric_std_unsigned.to_slv ; 
use ieee.std_logic_1164.all ; 
use std.textio.all ; 

entity to_slv_issue is
end entity to_slv_issue ;
architecture doit of to_slv_issue is
begin
  process 
    variable buf : line ; 
  begin
    for i in 9 to 17 loop 
      hwrite(buf, to_slv(i,8)) ;
      writeline(OUTPUT, buf) ; 
    end loop ;
    wait ; 
  end process ;
end architecture doit ; 



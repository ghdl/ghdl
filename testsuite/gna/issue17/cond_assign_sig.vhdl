library ieee ; 
use ieee.std_logic_1164.all ; 
use std.textio.all ; 

entity cond_assign_sig is
end entity cond_assign_sig ;

architecture doit of cond_assign_sig is
  signal Clk : std_logic := '0'  ; 
  signal Y : std_logic ; 
begin
  Clk <= not Clk after 10 ns ; 
  
  process (Clk)
  begin
    Y <= 'H' when Clk = '1' else 'L' ;
  end process ;

--    Y <= 'H' when Clk = '1' else 'L' ;
    
  process
  begin
    wait for 500 ns ; 
    std.env.stop ;
  end process ; 
end architecture doit ; 



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.TestPkg.all ;

entity test is
  port(
    input : in ARecType 
  );
end entity;

architecture rtl of test is
  alias B is input ; 
  
  signal Copy : ARecType ; 

begin
  copy <= input ;
  
  process 
  begin 
    wait on copy ;  -- Suppress first run
    report "Copy.A, B.A = " & to_string(Copy.A) & ", " & to_string(B.A) ; 
  end process ; 
  
end architecture;
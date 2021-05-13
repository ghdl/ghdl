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
  signal copy : ARecType(A(input.A'range)) ; -- Works
  
  alias B is copy ; 

begin
  copy <= input ;
  
  process 
  begin 
    wait on copy ;  -- Suppress first run
    report "Copy.A, B.A = " & to_hstring(Copy.A) & ", " & to_hstring(B.A) ; 
  end process ; 
  
end architecture;
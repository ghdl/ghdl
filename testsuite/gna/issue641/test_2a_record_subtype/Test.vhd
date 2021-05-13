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
  signal copy : input'subtype;
begin
  copy <= input ;
  
  process 
  begin 
    wait on copy ;  -- Suppress first run
    report "Copy.A = " & to_hstring(Copy.A) ; 
  end process ; 
  
end architecture;
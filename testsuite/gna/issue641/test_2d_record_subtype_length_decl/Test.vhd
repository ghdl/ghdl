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
  signal copy : input'subtype;   -- fails

  constant B : integer := Copy.A'length ; 
  
begin
  copy <= input ;
  
  process 
  begin 
    wait on copy ;  -- Suppress first run
    report "Copy.A, B = " & to_hstring(Copy.A) & ", " & to_string(B) ; 
  end process ; 
  
end architecture;
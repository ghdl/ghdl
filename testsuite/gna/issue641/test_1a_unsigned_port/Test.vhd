library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.TestPkg.all ;

entity test is
  port(
    input : in unsigned
  );
end entity;

architecture rtl of test is
  signal copy : input'subtype;
begin
  copy <= input ;
  
  process 
  begin 
    wait on copy ;  -- Suppress first run
    report "Copy = " & to_hstring(Copy) ; 
  end process ; 
  
end architecture;
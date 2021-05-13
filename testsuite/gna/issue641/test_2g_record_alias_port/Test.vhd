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

  signal copy : input'subtype;   
  
--  The following all fail, but the following error
-- .\tbtest:error: bound check failure at ../../src/ieee2008/std_logic_1164-body.vhdl:905
--  alias B : ARecType(A(input.A'range)) is input ;  
--  alias B : ARecType(A(7 downto 0)) is input ;  
--  subtype BType is ARecType(A(7 downto 0)) ;
--  alias B : BType is input ;
begin
  copy <= input ;
  
  process 
  begin 
    wait on copy ;  -- Suppress first run
    report "Copy.A, B.A = " & to_hstring(Copy.A) & ", " & to_hstring(B.A) ; 
  end process ; 
  
end architecture;
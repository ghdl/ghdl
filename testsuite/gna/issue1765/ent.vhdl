library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--use work.TestPkg.all ;

entity test is
  port(
    input  : in  unsigned ;
    output : out unsigned 
  );
end entity;

architecture rtl of test is
alias A is Output ; -- does not work
--  alias A : unsigned(output'range) is Output ;  -- Works
--  alias A : output'subtype is Output ;  -- Works

begin
  A <= (output'range => '0') ;
  
  process 
  begin 
    wait on input ;  -- Suppress first run
    report "input = " & to_hstring(input) ; 
  end process ; 
  
end architecture ;

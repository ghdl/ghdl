library ieee;
use ieee.float_pkg.all;

entity division_float is 
port(
i0 : in  float (7 downto -6);
i1 : in  float (7 downto -6);
p0 : out float (7 downto -6));
end division_float ;

architecture arch1 of division_float is 

begin
 p0 <= i0/i1 ;
end arch1;

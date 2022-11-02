library IEEE ;
use ieee.std_logic_1164.all ; 
use ieee.numeric_std.all ; 

entity PortMap2 is
end entity PortMap2 ;
architecture t1 of PortMap2 is 
  component fred is
    port (A : in signed(7 downto 0) ; B : out signed (7 downto 0) ) ; 
  end component fred ; 
  
  signal Result : signed (7 downto 0) ; 
begin

  fred1 :  fred 
    port map (A => signed'("00001111"),  B => Result ) ; 

end architecture t1 ;

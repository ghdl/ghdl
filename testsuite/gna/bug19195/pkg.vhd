library IEEE; 
use IEEE.std_logic_1164.all; 
package test_pkg is 
  type a is record 
    b : std_logic_vector(3 downto 0); 
  end record a; 
  type b is record 
    a1 : a; 
  end record b; 

  signal c : b; 
  alias c0 : a is c.a1; 
end package test_pkg; 
